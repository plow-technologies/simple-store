{-# LANGUAGE NoImplicitPrelude #-}

module SimpleStore.Types (SimpleStore(..)
                         ,StoreLock(..)
                         ,StoreError(..)) where

import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.TVar

import Prelude hiding (FilePath)
import Filesystem.Path

data SimpleStore st = SimpleStore
  { storeDir :: !(TVar FilePath)
  , storeState :: !(TVar st)
  , storeLock :: !(TMVar StoreLock)
  , storeCheckpointVersion :: !(TVar Int)
  }

data StoreLock =
  StoreLock

data StoreError
  = StoreAlreadyOpen
  | StoreClosed
  | StoreLocked
  | StoreFolderNotFound
  | StoreFileNotFound
  | NoStoreFilesInPath
  | StoreCheckpointNotFound
  | StoreIOError String
  deriving (Show, Eq)
