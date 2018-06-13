module SimpleStore (module SimpleStore) where

import           SimpleStore.IO    as SimpleStore (closeSimpleStore,
                                                   createCheckpoint,
                                                   createCheckpointImmediate,
                                                   getSimpleStore,
                                                   makeSimpleStore,
                                                   modifySimpleStore,
                                                   modifySimpleStoreResult,
                                                   modifySimpleStoreResultWith,
                                                   eitherModifySimpleStoreResultWith,
                                                   openSimpleStore,
                                                   attemptOpenDefault,
                                                   putSimpleStore)
import           SimpleStore.Types as SimpleStore (SimpleStore, StoreError (..))

import           SimpleStore.STM    as SimpleStore   (getSimpleStore',
                                                      putSimpleStore',
                                                      modifySimpleStore',
                                                      modifySimpleStoreResult',
                                                      modifySimpleStoreResultWith'
                                                      ) 
