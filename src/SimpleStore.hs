
{- |
Module      : SimpleStore
Description : Atomic, Durable Data Store 
Copyright   : Plow Technologies LLC

Maintainer  : Scott Murphy

SimpleStore consists of a very small api for generating serialization storage that is robust to
power pulls.  It also has write control locking.


SimpleStore works by maintaining a set of 4 files as checkpoints.
It cycles through these, writing a new one each time.  This
means you always have a backup that is closed on disk.

 -}
module SimpleStore ( closeSimpleStore
                   , createCheckpoint
                   , createCheckpointImmediate
                   , getSimpleStore
                   , makeSimpleStore
                   , modifySimpleStore
                   , openSimpleStore
                   , attemptOpenDefault
                   , putSimpleStore
                   , SimpleStore
                   , StoreError(..)
                   ) where

import       SimpleStore.IO      as SimpleStore (closeSimpleStore,
                                                    createCheckpoint,
                                                    createCheckpointImmediate,
                                                    getSimpleStore,
                                                    makeSimpleStore,
                                                    modifySimpleStore,
                                                    openSimpleStore,
                                                    attemptOpenDefault,
                                                    putSimpleStore)
import           SimpleStore.Types as SimpleStore (SimpleStore, StoreError (..))

