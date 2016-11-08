{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module SimpleStore.Internal (
    putWriteStore
  , obtainLock
  , releaseLock
  , withLock
  , processExists
  , getVersionNumber
  , createStore
  , isState
  , closeStoreHandle
) where

import           Control.Applicative
import           Control.Concurrent.STM.TMVar
import           Control.Concurrent.STM.TVar
import           Control.Exception
import           Control.Monad hiding (sequence)
import           Control.Monad.STM
import           Data.Bifunctor
import           Data.Text
import           Data.Text.Read
import           Filesystem.Path
import           Filesystem.Path.CurrentOS
import           Prelude                      hiding (FilePath, sequence)
import           SimpleStore.Types
import           System.Posix.Process
import           System.Posix.Types
import           System.IO                    (Handle, hClose)



putWriteStore :: SimpleStore st -> st -> IO ()
putWriteStore store state = atomically $ writeTVar tState state
  where tState = storeState store

-- | Lock a simplestore from being able to be written to
obtainLock :: SimpleStore st -> IO StoreLock
obtainLock store = atomically . takeTMVar . storeLock $ store

-- | Allow a simplestore to write to a lock
releaseLock :: SimpleStore st -> IO ()
releaseLock store = atomically $ putTMVar (storeLock store) StoreLock

-- | Run an IO function inside of a simple store lock
-- This is a cheap version of a transaction
withLock :: SimpleStore st -> IO b -> IO b
withLock store func = do
  _ <- obtainLock store
  finally func $ releaseLock store

-- | Check if a process exists
processExists :: Int -> IO Bool
processExists s = catch (getProcessPriority (CPid . fromIntegral $ s) >> return True) handleNotFound
  where
    handleNotFound :: IOException -> IO Bool
    handleNotFound _ = return False

-- Get the version number of a file from the filepath
getVersionNumber :: FilePath -> Either String Int
getVersionNumber fp = second fst $ join $ decimal <$> eTextFp
  where eTextFp = first unpack $ toText fp

-- Create a store from it's members. Just creates the necessary TMVars/TVars
createStore :: FilePath -> Handle -> Int -> st -> IO (SimpleStore st)
createStore fp fHandle version st = do
  sState <- newTVarIO st
  sLock <- newTMVarIO StoreLock
  sHandle <- newTMVarIO fHandle
  sVersion <- newTVarIO version
  sFp <- newTVarIO fp
  return $ SimpleStore sFp sState sLock sHandle sVersion

-- Checks the extension of a filepath for ".st"
isState :: FilePath -> Bool
isState fp = extension fp == Just "st"

-- Release the handle for a simplestore state file
closeStoreHandle :: SimpleStore st -> IO ()
closeStoreHandle store = do
  fHandle <- atomically . readTMVar . storeHandle $ store
  hClose fHandle
