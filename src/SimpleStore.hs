module SimpleStore (module SimpleStore) where

import           SimpleStore.IO    as SimpleStore (closeSimpleStore,
                                                   createCheckpoint,
                                                   createCheckpointImmediate,
                                                   getSimpleStore,
                                                   makeSimpleStore,
                                                   modifySimpleStore,
                                                   modifySimpleStoreResult,
                                                   openSimpleStore,
                                                   attemptOpenDefault,
                                                   putSimpleStore)
import           SimpleStore.Types as SimpleStore (SimpleStore, StoreError (..))
