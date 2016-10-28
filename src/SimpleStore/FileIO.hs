{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module SimpleStore.FileIO ( makeAbsoluteFp
                          , openNewestStore
                          , initializeDirectory
                          , createStoreFromFilePath
                          , checkpointBaseFileName
                          , checkpoint
                          , WithFsync(..)) where

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
import           SimpleStore.Internal
import           SimpleStore.Types
import           System.IO                 (hClose,hFlush,hPrint,stderr)
import           System.IO.Error
import           System.Posix.IO      (handleToFd,closeFd)
import           System.Posix.Unistd (fileSynchronise)

















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
              >> openNewestStore f (Prelude.filter (not . (== x)) xs)
    _ -> return res
  where  hIOException :: (FilePath -> IO (Either StoreError b)) ->  -- createNewStore
                         [FilePath] ->                            -- list of files to try                         
                         IOException ->                           -- io exceptions
                         IO (Either StoreError b)
         hIOException func ys e = hPrint stderr  e >>
                                  openNewestStore func (Prelude.filter (not . (== x)) ys)

-- Attempt to open a store from a filepath
createStoreFromFilePath :: (Serialize st) => FilePath -> IO (Either StoreError (SimpleStore st))
createStoreFromFilePath fp = do
  let eVersion = getVersionNumber . filename $ fp
  eFHandle <- try (openFile fp ReadWriteMode ) :: IO (Either SomeException Handle)
  eFConts  <- (either (return . Left) (try . BS.hGetContents) eFHandle) 
  putStrLn "createStoreFromFilepath"
  sequence $ toStoreIOError  $ createStore (directory fp)  <$> toStringErrorWithTag ("createStoreFromFilePath error: " ++  show fp ++ " ") eFHandle <*> 
                                                               eVersion               <*> 
                                                               (toStringErrorWithTag ("createStoreFromFilePath error: " ++  show fp ++ " ") eFConts >>= decode )




toStoreIOErrorWithTag :: String -> Either String c -> Either StoreError c
toStoreIOErrorWithTag str = first (StoreIOError . (++ str) . show)

toStringErrorWithTag :: String -> Either SomeException c -> Either String c
toStringErrorWithTag str= first ((++ str) . show)

toStoreIOError :: Either String c -> Either StoreError c
toStoreIOError = first (StoreIOError . show)

toStringError :: Either SomeException c -> Either String c
toStringError = first show

checkpointBaseFileName :: Text
checkpointBaseFileName = "checkpoint.st"






-- | If using Fsync on a file is enabled then run it on the given handle
data WithFsync = NoFsync | Fsync

withFsync :: WithFsync -> Handle -> IO ()
withFsync NoFsync _       = return ()
withFsync Fsync  oHandle  = do
  fd <- handleToFd oHandle
  fileSynchronise fd
  closeFd fd

-- | Create a checkpoint for a store. This attempts to write the state to disk
-- If successful it updates the version, releases the old file handle, and deletes the old file
checkpoint :: (Serialize st) => WithFsync -> SimpleStore st -> IO (Either StoreError ())
checkpoint fsync store = do

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

          updateIfWritten  l@(Left s) _       _       = putStrLn "updateIfWritten error: " *> print s *> pure l
          updateIfWritten  _ version' fHandle = do
            oHandle <- atomically $ writeTVar tVersion version' *>
                                    takeTMVar tHandle                                                  

            _       <- withFsync fsync oHandle
            _       <- atomically $ putTMVar tHandle fHandle
            _       <- hClose oHandle
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

