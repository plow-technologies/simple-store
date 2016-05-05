{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module SimpleStore.FileIO where

import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad             hiding (sequence)
import           Data.Bifunctor
import qualified Data.ByteString           as BS
import           Data.Serialize
import           Data.Text
import           Data.Traversable
import           Filesystem                hiding (readFile, writeFile)
import           Filesystem.Path
import           Filesystem.Path.CurrentOS hiding (decode, encode)
import           Prelude                   hiding (FilePath, sequence)
import           Safe
import           SimpleStore.Internal
import           SimpleStore.Types
import           System.IO                 (hClose,hFlush)
import           System.IO.Error
import           System.Posix.Process

-- | Return the given filepath if it is able to break the open.lock file
ableToBreakLock :: FilePath -> IO (Either StoreError FilePath)
ableToBreakLock fp = do
  fileExists <- isFile fp
  if fileExists
     then do
       ePid <- readMay <$> readFile (encodeString fp) :: IO (Maybe Int)
       case ePid of
         Just pid -> do
           exists <- processExists pid
           return $
             if exists
                then Left . StoreIOError $ "Process holding open.lock is already running"
                else Right fp
         Nothing -> return . Left . StoreIOError $ "Unable to parse open.lock"
     else return $ Right fp

-- | Catch all errors that allow the lock to still be taken
ableToBreakLockError :: IOError -> Bool
ableToBreakLockError = isDoesNotExistError

{- Old version of 'ableToBreakLockError'

ableToBreakLockError :: IOError -> Bool
ableToBreakLockError e
  | isAlreadyInUseError e = False
  | isDoesNotExistError e = True
  | isPermissionError e = False
  | otherwise = False

-}

-- | Create a lock file with the current process pid in it
-- The lock file should already be empty or non existent
createLock :: FilePath -> IO (Either StoreError ())
createLock fp = do
  pid <- getProcessID
  catch (Right <$> writeFile (encodeString fp) (show pid)) showError
  where showError :: IOException -> IO (Either StoreError ())
        showError e = return . Left . StoreIOError . show $ e

-- | Attempt to create a lock inside of the given filepath
attemptTakeLock :: FilePath -> IO (Either StoreError ())
attemptTakeLock baseFP = do
  let fp = baseFP </> (fromText "open.lock")
  allowBreak <- ableToBreakLock fp
  res <- sequence $ createLock <$> allowBreak
  return . join $ res

-- | release the lock for a given store
releaseFileLock :: SimpleStore st -> IO ()
releaseFileLock store = do
  fp <- (</> fromText "open.lock") <$> (atomically . readTVar . storeDir $ store)
  exists <- isFile fp
  when exists $ removeFile fp

-- Catch errors for storing so they aren't thrown
catchStoreError :: IOError -> StoreError
catchStoreError e
  | isAlreadyInUseError e = StoreAlreadyOpen
  | isDoesNotExistError e = StoreFileNotFound
  | isPermissionError e = StoreFileNotFound
  | otherwise = StoreIOError . show $ e

-- | Opens the newest store that doesn't throw an exception or give a StoreError back as a result
openNewestStore :: (a -> IO (Either StoreError b)) -> [a] -> IO (Either StoreError b)
openNewestStore _ [] = return . Left $ StoreFileNotFound
openNewestStore f (x:xs) = do
  res <- catch (f x) (hIOException f xs)
  case res of
    Left _ -> openNewestStore f xs
    _ -> return res
  where  hIOException :: (a -> IO (Either StoreError b)) -> [a] -> IOException -> IO (Either StoreError b)
         hIOException func ys _ = openNewestStore func ys

-- Attempt to open a store from a filepath
createStoreFromFilePath :: (Serialize st) => FilePath -> IO (Either StoreError (SimpleStore st))
createStoreFromFilePath fp = do
  let eVersion = getVersionNumber . filename $ fp
  fHandle <- openFile fp ReadWriteMode
  fConts <- BS.hGetContents fHandle
  sequence $ first (StoreIOError . show) $ (createStore (directory fp) fHandle) <$> 
                                           eVersion <*> 
                                           decode fConts

checkpointBaseFileName = "checkpoint.st"

-- | Create a checkpoint for a store. This attempts to write the state to disk
-- If successful it updates the version, releases the old file handle, and deletes the old file
checkpoint :: (Serialize st) => SimpleStore st -> IO (Either StoreError ())
checkpoint store = do
  fp <- readTVarIO . storeDir $ store
  state <- readTVarIO tState
  oldVersion <- readTVarIO tVersion
  let newVersion = (oldVersion + 1) `mod` 5
      olderVersion = (oldVersion - 1) `mod` 5 -- Marked for deletion
      encodedState = encode state
--       oldCheckpointPath = fp </> fromText  ( Data.Text.append (pack . show $ oldVersion)  checkpointBaseFileName)
      olderCheckpointPath = fp </> fromText  ( Data.Text.append (pack . show $ olderVersion)  checkpointBaseFileName)
      checkpointPath = fp </> fromText  ( Data.Text.append ( pack.show $ newVersion)  checkpointBaseFileName)
  newHandle <- openFile checkpointPath ReadWriteMode
  eFileRes <- catch (Right <$> BS.hPut newHandle encodedState) (return . Left . catchStoreError)  
  updateIfWritten olderCheckpointPath eFileRes newVersion newHandle
  where tState = storeState store
        tVersion = storeCheckpointVersion store
        tHandle = storeHandle store
        updateIfWritten _ l@(Left _) _ _= return l
        updateIfWritten older _ version fHandle = do
          oHandle <- atomically $ do
            writeTVar tVersion version
            oldHandle <- takeTMVar tHandle
            putTMVar tHandle fHandle
            return oldHandle
          hClose oHandle    
          hFlush fHandle
          yes <- isFile older
          if yes
            then removeFile older >> (return . Right) ()-- once removed file deletion                
            else return . Right $ ()

-- Initialize a directory by adding the working directory and checking if it already exists.
-- If the folder already exists it deletes it and creates a new directory
initializeDirectory :: FilePath -> IO FilePath
initializeDirectory dir = do
  fp <- makeAbsoluteFp dir
  exists <- isDirectory fp
  when exists $ removeTree fp
  createDirectory True fp
  return fp

makeAbsoluteFp :: FilePath -> IO FilePath
makeAbsoluteFp fp = do
  if absolute fp
    then return fp
    else do
      base <- getWorkingDirectory
      return $ base </> fp

