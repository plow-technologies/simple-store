{-# LANGUAGE NoImplicitPrelude #-}
module SimpleStore.Types where

import           Control.Concurrent.STM.TMVar
import           Control.Concurrent.STM.TVar
import           System.IO (Handle)
import Prelude hiding (FilePath)
import Filesystem.Path


data SimpleStore st = SimpleStore {
    storeDir               :: !(TVar FilePath)
  , storeState             :: !(TVar st)
  , storeLock              :: !(TMVar StoreLock)
  , storeHandle            :: !(TMVar Handle)
  , storeCheckpointVersion :: !(TVar Int)
}

data StoreLock = StoreLock

data StoreError = StoreAlreadyOpen
               | StoreClosed
               | StoreLocked
               | StoreFolderNotFound
               | StoreFileNotFound
               | NoStoreFilesInPath
               | StoreCheckpointNotFound
               | StoreIOError String deriving (Show, Eq)
