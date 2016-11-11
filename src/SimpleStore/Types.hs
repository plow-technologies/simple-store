{-# LANGUAGE NoImplicitPrelude #-}

{- |
Module      : SimpleStore.Types
Description : The Error type and defining structure of the store
Copyright   : Plow Technologies LLC

Maintainer  : Scott Murphy




 -}
module SimpleStore.Types (SimpleStore(..)
                         ,StoreLock(..)
                         ,StoreError(..)) where

import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.TVar

import Prelude hiding (FilePath)
import Filesystem.Path


-- | A SimpleStore will be created at the directory given
-- The storeDir is a filepath to the current active state's backup.
-- The store state is the in-memory representation of the item in storage.
-- The lock is engaged on modify and writes.
-- The checkpoint version says what to increment the next backup to.

data SimpleStore st = SimpleStore
  { storeDir   :: !(TVar FilePath)
  , storeState :: !(TVar st)
  , storeLock  :: !(TMVar StoreLock)
  , storeCheckpointVersion :: !(TVar Int)
  }

-- | Lock stores during writes
data StoreLock =
  StoreLock

-- | All error types for SimpleStore
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
