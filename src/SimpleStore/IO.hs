{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module SimpleStore.IO where

import           Control.Applicative
import           Control.Concurrent.STM.TVar
import           Control.Monad                hiding (sequence)
import           Control.Monad.STM
import           Data.Function
import           Data.List
import qualified Data.Serialize               as S
import           Data.Text                    hiding (filter, foldl, map,
                                               maximum, stripPrefix)
import qualified Data.Text.IO as Text                 
import           Data.Traversable
import           Filesystem
import           Filesystem.Path
import           Filesystem.Path.CurrentOS    hiding (decode)
import           Prelude                      hiding (FilePath, sequence,
                                               writeFile)
import Control.Concurrent.Async
import Control.Concurrent 
import           SimpleStore.FileIO
import           SimpleStore.Internal
import           SimpleStore.Types
import Data.Either
import Data.Time.Clock.POSIX 


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
  dir <- makeAbsoluteFp fp
  exists <- isDirectory dir
  if exists
     then do lock <- attemptTakeLock fp
             if isRight lock
                then do dirContents <- listDirectory dir
                        let lastTouch = (dir </> "last.touch")
                        lastTouchExists <- isFile lastTouch                        
                        let files = filter isState dirContents
                        modifiedDates <-
                           traverse (\file -> do               -- Lambda is because the instance for Traversable on ()
                                        t <- getModified file  -- Traverses the second item so sequence only evaluates
                                        
                                        return (t,file)        -- the second item                                        
                                       ) files                         
                        let
                          times = utcTimeToPOSIXSeconds . fst <$> modifiedDates
                          sortedDates = snd <$> sortBy (compare `on` fst) modifiedDates
                        if lastTouchExists 
                        then  do
                         (_, fpExpected) <- do
                                  let readLastTouch = (fmap fromText . Text.readFile $ encodeString $  lastTouch)
                                      defaultToNewest :: IO FilePath
                                      defaultToNewest 
                                        | Prelude.null sortedDates = fail "no state file found"
                                        | otherwise =  return $ Prelude.head $ Prelude.reverse $ sortedDates



                                  asyncLastTouch   <- async readLastTouch
                                  asyncDefaultToNewest <- async $ (threadDelay (3 * 10^6 ) >> defaultToNewest)
                                  waitAny [asyncLastTouch, asyncDefaultToNewest]
                         




                         openNewestStore createStoreFromFilePath ((fpExpected) : Prelude.reverse sortedDates)
                        else
                          openNewestStore createStoreFromFilePath ( Prelude.reverse sortedDates)
                else return . Left $ StoreLocked
     else return . Left $ StoreFolderNotFound


   
-- | Initialize a simple store from a given filepath and state.
-- The filepath should just be to the directory you want the state created in
-- as in "state"
makeSimpleStore :: (S.Serialize st) => FilePath -> st -> IO (Either StoreError (SimpleStore st))
makeSimpleStore dir state = do
  fp <- initializeDirectory dir
  _ <- attemptTakeLock fp
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
  releaseFileLock store

-- | Run a function against the state and put the result into the state
-- This does not write the store to disk
modifySimpleStore :: SimpleStore st -> (st -> IO st) -> IO (Either StoreError ())
modifySimpleStore store modifyFunc = withLock store $ do
  res <- modifyFunc =<< readTVarIO tState
  Right <$> (atomically $ writeTVar tState res)
  where tState = storeState store


-- | Write the current store to disk in the given folder
createCheckpoint :: (S.Serialize st) => SimpleStore st -> IO (Either StoreError ())
createCheckpoint store = withLock store $ checkpoint store

