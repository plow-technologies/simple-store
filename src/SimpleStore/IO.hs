{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module SimpleStore.IO
  ( putSimpleStore
  , getSimpleStore
  , openSimpleStore
  , createCheckpoint
  , createCheckpointImmediate
  , closeSimpleStore
  , modifySimpleStore
  , modifySimpleStoreResult
  , modifySimpleStoreResultWith
  , makeSimpleStore
  , attemptOpenDefault
  ) where

import Control.Concurrent.STM.TVar (readTVarIO
                                   ,readTVar
                                   ,writeTVar)

import Control.Monad hiding (sequence)
import Control.Monad.STM (atomically)

import Data.Function (on)
import Data.List (sortBy)
import qualified Data.Serialize as S
import Data.Text hiding (filter, foldl, map, maximum, stripPrefix)




import Filesystem
       (isDirectory, getModified, listDirectory, isFile)
import Filesystem.Path.CurrentOS
       (FilePath, (</>),  fromText)
import Prelude
       (IO, (.), ($), Either(..), (<$>), snd, fst, compare, traverse,
        filter, null, pure, last, putStrLn, (++), show, reverse,
        (+), either,otherwise)

import Data.Time (UTCTime)
import SimpleStore.FileIO
import SimpleStore.Internal
import SimpleStore.Types

import System.AtomicWrite.Writer.ByteString

import Filesystem.Path.CurrentOS (encodeString)
import Data.Function (const)

-- | Get the current value of the store
getSimpleStore :: SimpleStore st -> IO st
getSimpleStore store = atomically . readTVar . storeState $ store

-- | Put a new value into a simple store with the lock
putSimpleStore :: SimpleStore st -> st -> IO ()
putSimpleStore store state = withLock store $ putWriteStore store state

-- | Open a simple store from a filepath reading in the newest most valid store
-- important to the operation of this is the last.touch file.
-- this file tells openSimpleStore what to try and open first.
-- it is plain text encoded and updated on every checkpoint.
openSimpleStore
  :: S.Serialize st
  => FilePath -> IO (Either StoreError (SimpleStore st))
openSimpleStore fp = do
  dir <- makeAbsoluteFp fp
  exists <- isDirectory dir
  if exists
    then openStoreFound dir
    else return . Left $ StoreFolderNotFound
  where
    isSTPrefixedFile = isState
    lastTouch dir    = dir </> "last.touch"

    sortModifiedDateTuples modifiedDates =
      snd <$> sortBy (compare `on` fst) modifiedDates

    timeSortFiles dirContents =
      sortModifiedDateTuples <$>
      traverse buildModifiedDateTuple (filter isSTPrefixedFile dirContents)

    buildModifiedDateTuple :: FilePath -> IO (UTCTime, FilePath) -- time last modified and file being referred to
    buildModifiedDateTuple file = (, file) <$> getModified file

    -- If something goes wrong with another method of searching for the latest file
    -- use the modified date on the OS
    defaultToNewest :: [FilePath] -> IO FilePath
    defaultToNewest filesSortedByTouchTime
      | Prelude.null filesSortedByTouchTime = fail "no state file found"
      | otherwise                           = pure $ Prelude.last filesSortedByTouchTime

    openStoreFound dir = do
      dirContents            <- listDirectory dir
      filesSortedByTouchTime <- timeSortFiles dirContents
      lastTouchExists        <- (isFile . lastTouch) dir
      if lastTouchExists
        then do
          fpExpected <- readLastTouch dir (defaultToNewest filesSortedByTouchTime)
          putStrLn $ "file path: " ++ (show fpExpected)
          openNewestStore
            createStoreFromFilePath
            ((fpExpected) : Prelude.reverse filesSortedByTouchTime)
        else openNewestStore
               createStoreFromFilePath
               (Prelude.reverse filesSortedByTouchTime)

-- | Initialize a simple store from a given filepath and state.
-- The filepath should just be to the directory you want the state created in
-- as in "state"
makeSimpleStore
  :: (S.Serialize st)
  => FilePath -> st -> IO (Either StoreError (SimpleStore st))
makeSimpleStore dir state = do
  fp <- initializeDirectory dir
  let encodedState = S.encode state
      checkpointPath =
        fp </>
        (fromText . pack $
         (show $ 1 + initialVersion) ++ (unpack checkpointBaseFileName))
      checkpointPathBackup =
        fp </>
        (fromText . pack $
         (show $ initialVersion) ++
         (unpack checkpointBaseFileName) -- we write a backup immediately
         )
      initialVersion = 0
  atomicWriteFile (encodeString checkpointPath) encodedState
  atomicWriteFile (encodeString checkpointPathBackup) encodedState
  Right <$> createStore fp (1 + initialVersion) state

-- | Attempt to open a store. If the store doesn't it exist it will create the store in the filepath given
-- with makeSimpleStore.
attemptOpenDefault
  :: (S.Serialize st)
  => FilePath -> st -> IO (Either StoreError (SimpleStore st))
attemptOpenDefault fp initialState = do
  eStore <- openSimpleStore fp
  either (\_ -> makeSimpleStore fp initialState) (return . Right) eStore

-- | Release the file lock and close the handle to the file allowing another processes to open
-- the store
closeSimpleStore :: a -> IO ()
closeSimpleStore _ = putStrLn "close Simple Store is deprecated" >> return ()

-- | Run a function against the state and put the result into the state
-- This does not write the store to disk
modifySimpleStore :: SimpleStore st
                  -> (st -> IO st)
                  -> IO (Either StoreError ())
modifySimpleStore store modifyFunc =
  (fmap (const ())) <$> modifySimpleStoreResult store modifyFunc

-- | Modify a simple store with a function that
-- computes a new state
modifySimpleStoreResult :: SimpleStore st
                        -> (st -> IO st)
                        -> IO (Either StoreError st)
modifySimpleStoreResult store modifyFunc =
  (fmap fst) <$> modifySimpleStoreResultWith store (fmap (\x -> (x,())) . modifyFunc)


-- | Modify a simple store internal value and
-- and return a value computed inside the modify function
modifySimpleStoreResultWith :: SimpleStore st
                            -> (st -> IO (st, a))
                            -> IO (Either StoreError (st, a))
modifySimpleStoreResultWith store modifyFunc =
  withLock store $ do
    res@(st', _) <- modifyFunc =<< readTVarIO tState
    atomically $ writeTVar tState st'
    return $ Right res
  where
    tState = storeState store


-- | Write the current store to disk in the given folder
createCheckpoint
  :: (S.Serialize st)
  => SimpleStore st -> IO (Either StoreError ())
createCheckpoint store = withLock store $ checkpoint NoFsync store

-- | Write the current store to disk in the given folder immediately
createCheckpointImmediate
  :: (S.Serialize st)
  => SimpleStore st -> IO (Either StoreError ())
createCheckpointImmediate store = withLock store $ checkpoint Fsync store
