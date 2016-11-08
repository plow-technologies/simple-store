{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module SimpleStore.FileIO
  ( makeAbsoluteFp
  , openNewestStore
  , initializeDirectory
  , createStoreFromFilePath
  , checkpointBaseFileName
  , checkpoint
  , readLastTouch
  , WithFsync(..)
  ) where

import Control.Concurrent.STM (readTVarIO, writeTVar, atomically)
import Control.Exception (catch, IOException, try, SomeException)
import Control.Monad (return, (>>), fail, (>>=))
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import Data.Text.Encoding (decodeUtf8')
import Data.Serialize (Serialize, decode, encode)
import Data.Text (append, Text, pack)
import qualified Data.Text.IO as Text
import Data.Traversable (sequence, traverse)
import Filesystem
       (openFile, IOMode(..), Handle, isDirectory, createDirectory,
        getWorkingDirectory)

import Filesystem.Path.CurrentOS
       (FilePath, filename, (</>), directory, fromText, encodeString,
        absolute)
import Prelude
       (Either(..), (.), ($), (++), show, Bool(..), print, mod, (/=),
        (*>), String, IO, otherwise, putStrLn, appendFile, filter, not,
        (==), (<$>), (+), either, (<*>), Int, error, pure)
import SimpleStore.Internal
import SimpleStore.Types (StoreError(..), SimpleStore(..))
import System.IO (hClose, hPrint, stderr)
import System.IO.Error
       (IOError, isAlreadyInUseError, isDoesNotExistError,
        isPermissionError, tryIOError)
import System.Posix.IO (handleToFd, closeFd)

import System.Posix.Unistd (fileSynchronise)

-- --------------------------------------------------
-- API
-- --------------------------------------------------
-- | Opens the newest store that doesn't throw an exception or give a StoreError back as a result
openNewestStore :: (FilePath -> IO (Either StoreError b))
                -> [FilePath]
                -> IO (Either StoreError b)
openNewestStore _ [] = return . Left $ NoStoreFilesInPath
openNewestStore f (x:xs) = do
  res <- catch (f x) (hIOException f xs)
  case res of
    Left e ->
      putStrLn "errors found and written to errors.log" >>
      (appendFile "errors.log" ("filePath:" ++ show x ++ "\n error: " ++ show e)) >>
      openNewestStore f (Prelude.filter (not . (== x)) xs)
    _ -> return res
  where
    hIOException
      :: (FilePath -> IO (Either StoreError b)) -- createNewStore
      -> [FilePath]                             -- list of files to try
      -> IOException                            -- io exceptions
      -> IO (Either StoreError b)
    hIOException func ys e =
      hPrint stderr e >> openNewestStore func (Prelude.filter (not . (== x)) ys)
    


-- Attempt to open a store from a filepath
createStoreFromFilePath
  :: (Serialize st)
  => FilePath -> IO (Either StoreError (SimpleStore st))
createStoreFromFilePath fp = do
  let eVersion = getVersionNumber . filename $ fp
  eFHandle <-
    try (openFile fp ReadWriteMode) :: IO (Either SomeException Handle)
  eFConts <- (either (return . Left) (try . BS.hGetContents) eFHandle)
  eitherStore <-
    sequence $
    toStoreIOError $
    createStore (directory fp) <$> eVersion <*>
    (toStringErrorWithTag
       ("createStoreFromFilePath contents error: " ++ show fp ++ " ")
       eFConts >>=
     decode)
  case eitherStore of
    r@(Right _) -> return r
    l@(Left _) -> hClose `traverse` eFHandle *> return l

toStringErrorWithTag :: String -> Either SomeException c -> Either String c
toStringErrorWithTag str = first ((++ str) . show)

toStoreIOError :: Either String c -> Either StoreError c
toStoreIOError = first (StoreIOError . show)

checkpointBaseFileName :: Text
checkpointBaseFileName = "checkpoint.st"

-- | If using Fsync on a file is enabled then run it on the given handle
data WithFsync
  = NoFsync
  | Fsync

withFsyncCheck :: WithFsync -> Handle -> IO ()
withFsyncCheck NoFsync handle' = hClose handle'
withFsyncCheck Fsync handle' = do
  fd <- handleToFd handle'
  eitherE <- tryIOError (fileSynchronise fd)
  eitherE2 <- tryIOError (closeFd fd)
  case concatShow <$> eitherE <*> eitherE2 of
    (Left e) -> putStrLn "withFsyncCheck Failure: " >> print e
    (Right _) -> return ()
  where
    concatShow x y = (show x) ++ (show y)


-- | Create a checkpoint for a store. This attempts to write the state to disk
-- If successful it updates the version, releases the old file handle, and deletes the old file
checkpoint
  :: (Serialize st)
  => WithFsync -> SimpleStore st -> IO (Either StoreError ())
checkpoint fsync store = do
  !fp <- readTVarIO . storeDir $ store
  !state <- readTVarIO . storeState $ store
  !oldVersion <- readTVarIO tVersion
  let !newVersion = (oldVersion + 1) `mod` 5 :: Int
      !encodedState = encode state :: BS.ByteString
      newFileName =
        (Data.Text.append (pack . show $ newVersion) checkpointBaseFileName)
      oldFileName =
        (Data.Text.append (pack . show $ oldVersion) checkpointBaseFileName)
      newCheckpointPath
        | oldFileName /= newFileName = fp </> fromText newFileName
        | otherwise = error "old filename and new file name must never match"
  !newHandle <- openFile newCheckpointPath WriteMode -- new checkpoint creation
  !eFileRes <-
    catch
      (Right <$> BS.hPut newHandle encodedState)
      (return . Left . catchStoreError)
  eVal <- updateIfWritten eFileRes newVersion
  _ <- writeNewLastTouchValue fp newCheckpointPath -- want to write after we know everything worked
  withFsyncCheck fsync newHandle
  return eVal
  where
    writeNewLastTouchValue fp newCheckpointPath =
      Text.writeFile
        (encodeString $ fp </> "last.touch")
        (pack . encodeString $ newCheckpointPath)
    tVersion = storeCheckpointVersion store
    updateIfWritten l@(Left s) _ =
      putStrLn "updateIfWritten error: " *> print s *> pure l
    updateIfWritten _ version' = do
      _ <- atomically $ writeTVar tVersion version'
      return $ Right ()

-- Initialize a directory by adding the working directory and checking if it already exists.
-- If the folder already exists it deletes it and creates a new directory
initializeDirectory :: FilePath -> IO FilePath
initializeDirectory dir = do
  fp <- makeAbsoluteFp dir
  exists <- isDirectory fp
  stopIfDirectoryPresent exists fp
  where
    stopIfDirectoryPresent exists fp
      | exists = fail (show fp ++ "exists already, failing")
      | otherwise = createDirectory True fp *> return fp

makeAbsoluteFp :: FilePath -> IO FilePath
makeAbsoluteFp fp
  | absolute fp = return fp
  | otherwise = (</> fp) <$> getWorkingDirectory

-- --------------------------------------------------
-- Helpers
--------------------------------------------------
-- Catch errors for storing so they aren't thrown
catchStoreError :: IOError -> StoreError
catchStoreError e
  | isAlreadyInUseError e = StoreAlreadyOpen
  | isDoesNotExistError e = StoreFileNotFound
  | isPermissionError e = StoreFileNotFound
  | otherwise = StoreIOError . show $ e



readLastTouch :: FilePath -> IO FilePath -> IO FilePath
readLastTouch dir failoverAction = performRead >>=
                                   (evaluateResult.decodeResult)

  where
    lastTouch = dir </> "last.touch"
    decodeResult               = (>>= first decodeFileError  . decodeUtf8') . first readFileError
    
    evaluateResult (Left  err) = recordError err *> failoverAction
    evaluateResult (Right txt) = return . fromText $ txt
    
    recordError     e = appendFile "errors.log" ("filePath:" ++ show  lastTouch  ++ "\n error: " ++ show e)    
    readFileError   e =  "error reading file "         ++ show e
    decodeFileError e =  "error parsing text of file " ++ show e
    
    performRead = do
           let stringFilePath = encodeString lastTouch  :: String -- Create the full filepath for last.touch
           try $ BS.readFile stringFilePath :: IO (Either SomeException BS.ByteString)
