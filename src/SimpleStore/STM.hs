{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module SimpleStore.STM
  ( getSimpleStore'
  , putSimpleStore'
  , modifySimpleStore'
  , modifySimpleStoreResult'
  , modifySimpleStoreResultWith'
  ) where

import Prelude (fst, ($), (.), (<$>))

import Control.Concurrent.STM.TVar (readTVar, writeTVar)
import Control.Monad hiding (sequence)
import Control.Monad.STM (STM)

import SimpleStore.Types

-- | Get the current value of the store
getSimpleStore' :: SimpleStore st -> STM st
getSimpleStore' store = readTVar . storeState $ store

-- | Put a new value into a simple store with the lock
putSimpleStore' :: SimpleStore st -> st -> STM ()
putSimpleStore' store state = writeTVar tState state
  where tState = storeState store

-- | Run a function against the state and put the result into the state
-- This does not write the store to disk
modifySimpleStore' :: SimpleStore st
                   -> (st -> st)
                   -> STM ()
modifySimpleStore' store modifyFunc =
  void $ modifySimpleStoreResult' store modifyFunc

-- | Modify a simple store with a function that
-- computes a new state
modifySimpleStoreResult' :: SimpleStore st
                         -> (st -> st)
                         -> STM st
modifySimpleStoreResult' store modifyFunc =
  fst <$> modifySimpleStoreResultWith' store (\st -> (modifyFunc st, ()))


-- | Modify a simple store internal value and
-- and return a value computed inside the modify function
modifySimpleStoreResultWith' :: SimpleStore st
                             -> (st -> (st, a))
                             -> STM (st, a)
modifySimpleStoreResultWith' store modifyFunc = do
  res <- modifyFunc <$> readTVar tState
  writeTVar tState $ fst res
  return res
  where
    tState = storeState store
