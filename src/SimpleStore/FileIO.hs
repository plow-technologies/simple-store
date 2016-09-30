{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module SimpleStore.FileIO where

import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad             hiding (sequence)
import           Data.Bifunctor
import qualified Data.ByteString           as BS
import           Data.Serialize
import           Data.Text
import qualified Data.Text.IO as Text
import           Data.Traversable
import           Filesystem                hiding (readFile, writeFile)
import           Filesystem.Path
import           Filesystem.Path.CurrentOS hiding (decode, encode)
import           Prelude                   hiding (FilePath, sequence)
import           Safe
import           SimpleStore.Internal
import           SimpleStore.Types
import           System.IO                 (hClose,hFlush,hPrint,stderr)
import           System.IO.Error
import           System.Posix.IO
import           System.Posix.Process
import           System.Posix.Unistd








-- | Return the given filepath if it is able to break the open.lock file
ableToBreakLock :: FilePath -> IO (Either StoreError FilePath)
ableToBreakLock fp = do
  !fileExists <- isFile fp
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
  fp     <- (</> fromText "open.lock") <$> (atomically . readTVar . storeDir $ store)
  exists <- isFile fp
  when exists $ removeFile fp







-- Catch errors for storing so they aren't thrown
catchStoreError :: IOError -> StoreError
catchStoreError e
  | isAlreadyInUseError e = StoreAlreadyOpen
  | isDoesNotExistError e = StoreFileNotFound
  | isPermissionError   e = StoreFileNotFound
  | otherwise             = StoreIOError . show $ e







-- | Opens the newest store that doesn't throw an exception or give a StoreError back as a result
openNewestStore :: (FilePath -> IO (Either StoreError b)) -> [FilePath] -> IO (Either StoreError b)
openNewestStore _ [] = return . Left $ NoStoreFilesInPath
openNewestStore f (x:xs) = do
  res <- catch (f x) (hIOException f xs)
  case res of
    Left e -> putStrLn "errors found and written to errors.log" >>
              (writeFile "errors.log" ("filePath:" ++  show x ++ "\n error: " ++ show e))
              >> openNewestStore f xs
    _ -> return res
  where  hIOException :: (FilePath -> IO (Either StoreError b)) ->  -- createNewStore
                         [FilePath] ->                            -- list of files to try                         
                         IOException ->                           -- io exceptions
                         IO (Either StoreError b)
         hIOException func ys e = hPrint stderr  e >>
                                  openNewestStore func ys

-- Attempt to open a store from a filepath
createStoreFromFilePath :: (Serialize st) => FilePath -> IO (Either StoreError (SimpleStore st))
createStoreFromFilePath fp = do
  let eVersion = getVersionNumber . filename $ fp
  eFHandle <- try (openFile fp ReadWriteMode ) :: IO (Either SomeException Handle)
  eFConts  <- (either (return . Left) (try . BS.hGetContents) eFHandle) 
  putStrLn "createStoreFromFilepath"
  sequence $ toStoreIOError  $ createStore (directory fp)  <$> toStringError eFHandle <*> 
                                                               eVersion               <*> 
                                                               (toStringError eFConts >>= decode )


toStoreIOError :: Either String c -> Either StoreError c
toStoreIOError = first (StoreIOError . show)

toStringError :: Either SomeException c -> Either String c
toStringError = first show

checkpointBaseFileName :: Text
checkpointBaseFileName = "checkpoint.st"






-- | Create a checkpoint for a store. This attempts to write the state to disk
-- If successful it updates the version, releases the old file handle, and deletes the old file
checkpoint :: (Serialize st) => SimpleStore st -> IO (Either StoreError ())
checkpoint store = do
  
  !fp         <- readTVarIO . storeDir $ store
  !state      <- readTVarIO tState
  !oldVersion <- readTVarIO tVersion

  let !newVersion         = (oldVersion + 1) `mod` 5
      !encodedState       = encode state      
      newFileName         =  ( Data.Text.append ( pack.show   $ newVersion )
                                                  checkpointBaseFileName   )                    
      checkpointPath      = fp </> fromText  newFileName



  newHandle <- openFile checkpointPath WriteMode
  _         <- Text.writeFile (encodeString $ fp </> "last.touch")  (pack.encodeString $ checkpointPath)
  !eFileRes <- catch (Right <$> BS.hPut newHandle encodedState)
                     (return . Left . catchStoreError)
               
  updateIfWritten eFileRes newVersion newHandle  
    where
      
          tState   = storeState             store
          tVersion = storeCheckpointVersion store
          tHandle  = storeHandle            store

          updateIfWritten  l@(Left _) _       _       = return l
          updateIfWritten  _ version fHandle = do
            oHandle <- atomically $ do            
                         _         <- writeTVar tVersion version
                         oldHandle <- takeTMVar tHandle
                         _         <- putTMVar  tHandle fHandle
                         return oldHandle

            _       <- hClose oHandle
            _       <- fileSynchronise =<< handleToFd oHandle
            _       <- hFlush fHandle
            _       <- fileSynchronise =<< handleToFd fHandle
            return $ Right ()








-- Initialize a directory by adding the working directory and checking if it already exists.
-- If the folder already exists it deletes it and creates a new directory
initializeDirectory :: FilePath -> IO FilePath
initializeDirectory dir = do
  fp <- makeAbsoluteFp dir
  exists <- isDirectory fp
  when exists $ fail (show fp ++ "exists already, failing") 
  createDirectory True fp
  return fp







makeAbsoluteFp :: FilePath -> IO FilePath
makeAbsoluteFp fp = do
  if absolute fp
    then return fp
    else do
      base <- getWorkingDirectory
      return $ base </> fp

