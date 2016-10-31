{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module SimpleStore.IO where

import           Control.Applicative
import           Control.Concurrent.STM.TVar
import           Control.Monad                hiding (sequence)
import           Control.Monad.STM
import           Control.Exception (SomeException, try)
import           Data.Function
import           Data.List
import qualified Data.Serialize               as S
import           Data.Text                    hiding (filter, foldl, map,
                                               maximum, stripPrefix)

import Data.Text.Encoding (decodeUtf8')
import qualified Data.ByteString as B
import           Data.Traversable
import           Filesystem
import           Filesystem.Path
import           Filesystem.Path.CurrentOS    hiding (decode)
import           Prelude                      hiding (FilePath, sequence,
                                               writeFile)


import           Data.Time                   (UTCTime)
import           SimpleStore.FileIO
import           SimpleStore.Internal
import           SimpleStore.Types
import Data.Either



-- | Get the current value of the store
getSimpleStore :: SimpleStore st -> IO st
getSimpleStore store = atomically . readTVar . storeState $ store

-- | Put a new value into a simple store with the lock
putSimpleStore :: SimpleStore st -> st -> IO ()
putSimpleStore store state = withLock store $ putWriteStore store state

-- | Open a simple store from a filepath reading in the newest most valid store
-- important to the operation of this is the last.touch file.
-- this file tells openSimpleStore where to try and open first.
-- it is plain text encoded and updated on every checkpoint.
openSimpleStore :: S.Serialize st => FilePath -> IO (Either StoreError (SimpleStore st))
openSimpleStore fp = do
  dir    <- makeAbsoluteFp fp
  exists <- isDirectory dir
  if exists
     then  openStoreFound dir
     else return . Left $ StoreFolderNotFound
  where     
     isSTPrefixedFile    = isState
     lastTouch      dir  = dir </> "last.touch"
     sortModifiedDateTuples modifiedDates = snd <$> sortBy (compare `on` fst) modifiedDates
     timeSortFiles dirContents          = sortModifiedDateTuples <$>
                                          traverse buildModifiedDateTuple (filter isSTPrefixedFile dirContents)
     buildModifiedDateTuple :: FilePath -> IO (UTCTime, FilePath)  -- time last modified and file being referred to 
     buildModifiedDateTuple file = (,file) <$> getModified file
     
     defaultToNewest :: [FilePath] -> IO FilePath
     defaultToNewest filesSortedByTouchTime = do
       if Prelude.null filesSortedByTouchTime
       then fail "no state file found"
       else pure $ Prelude.last filesSortedByTouchTime

     openStoreFound dir  = do       dirContents              <- listDirectory        dir
                                    filesSortedByTouchTime   <- timeSortFiles dirContents    
                                    lastTouchExists          <- (isFile . lastTouch) dir                                         
                                    if lastTouchExists 
                                    then  do
                                     fpExpected <- do                                                   
                                              let stringFilePath = encodeString (lastTouch dir) :: String -- Create the full filepath for last.touch
                                              binaryContent <- try $ B.readFile stringFilePath :: IO (Either SomeException B.ByteString)
                                              -- Decode bytestring as text
                                              case decodeUtf8' <$> binaryContent of                                                     
                                               Left err -> do
                                               -- There was an error reading the file
                                                 putStrLn $ "Error reading file " ++ stringFilePath ++ ": " ++ show err
                                                 defaultToNewest filesSortedByTouchTime
                                               Right etext ->
                                                 -- Bytes were loaded successfully 
                                                 case etext of
                                                   Left err -> do
                                                     -- There was an error decoding the bytes as text
                                                     putStrLn $ "Error parsing text of file " ++ stringFilePath ++ ": " ++ show err
                                                     defaultToNewest filesSortedByTouchTime
                                                   -- File was parsed as text successfully
                                                   Right text -> pure $ fromText text
                                     putStrLn $ "file path: " ++ (show fpExpected)
                                     openNewestStore createStoreFromFilePath ((fpExpected) : Prelude.reverse filesSortedByTouchTime)
                                    else
                                      openNewestStore createStoreFromFilePath ( Prelude.reverse filesSortedByTouchTime)
                                   





-- | Initialize a simple store from a given filepath and state.
-- The filepath should just be to the directory you want the state created in
-- as in "state"
makeSimpleStore :: (S.Serialize st) => FilePath -> st -> IO (Either StoreError (SimpleStore st))
makeSimpleStore dir state = do
  fp <- initializeDirectory dir
  let encodedState = S.encode state
      checkpointPath = fp </> (fromText . pack $ (show $ 1 + initialVersion) ++ (unpack checkpointBaseFileName))
      checkpointPathBackup  = fp </> (fromText . pack $ (show $ initialVersion) ++
                                                        (unpack checkpointBaseFileName)) -- we write a backup immediately
      initialVersion = 0
  writeFile checkpointPath encodedState
  writeFile checkpointPathBackup encodedState  
  handle <- openFile checkpointPath ReadWriteMode
  Right <$> createStore fp handle (1 + initialVersion) state


-- | Attempt to open a store. If the store doesn't it exist it will create the store in the filepath given
-- with makeSimpleStore.
attemptOpenDefault :: (S.Serialize st) => FilePath -> st -> IO (Either StoreError (SimpleStore st))
attemptOpenDefault fp initialState = do
  eStore <- openSimpleStore fp
  either (\_ -> makeSimpleStore fp initialState) (return . Right) eStore



-- | Release the file lock and close the handle to the file allowing another processes to open
-- the store
closeSimpleStore :: SimpleStore st -> IO ()
closeSimpleStore store = withLock store $ do
  closeStoreHandle store

-- | Run a function against the state and put the result into the state
-- This does not write the store to disk
modifySimpleStore :: SimpleStore st -> (st -> IO st) -> IO (Either StoreError ())
modifySimpleStore store modifyFunc = withLock store $ do
  res <- modifyFunc =<< readTVarIO tState
  Right <$> (atomically $ writeTVar tState res)
  where tState = storeState store


-- | Write the current store to disk in the given folder
createCheckpoint :: (S.Serialize st) => SimpleStore st -> IO (Either StoreError ())
createCheckpoint store = withLock store $ checkpoint NoFsync store



-- | Write the current store to disk in the given folder immediately
createCheckpointImmediate :: (S.Serialize st) => SimpleStore st -> IO (Either StoreError ())
createCheckpointImmediate store = withLock store $ checkpoint Fsync store
