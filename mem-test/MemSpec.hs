{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}



-- import           Control.Concurrent

import GHC.Generics 
import Data.Serialize
import Control.Monad
-- import           Control.Concurrent.STM.TMVarp
import Control.Concurrent (threadDelay, forkIO)
import           Data.Either
import           Data.Traversable
-- import           Data.Traversable
import Control.Applicative
import           Filesystem
import           Filesystem.Path
import           Filesystem.Path.CurrentOS
import           Prelude                      hiding (sequence)
import           SimpleStore



newtype TestNum = TestNum { _unNum :: Int }
 deriving (Generic,Show,Eq,Num,Integral,Real,Enum,Ord)

instance Serialize TestNum where 


intList :: [TestNum]
intList = TestNum <$> [0 .. 100]

testTreeDir :: Filesystem.Path.CurrentOS.FilePath
testTreeDir = "xkcd-test-states"

main :: IO () 
main = do dataSet <- traverse makeTestStores  intList
          putStrLn "starting memory loop"
          _ <- traverse (\l -> forkIO (runMemTestLoop l ) ) dataSet           
          removeTree testTreeDir
          return ()


runMemTestLoop
  :: (Show c, Serialize c, Integral c)
  => (Either a (SimpleStore c), b1, c, d)
  -> IO b2
runMemTestLoop  (eStore, dir, _x, workingDir ) = do 
       threadDelay (1*1000*1000)
       _st <- traverse getSimpleStore eStore
       case eStore of
               (Left _err) -> fail "Unable to open local state"
               (Right store) -> do
                     x' <- getSimpleStore store
                     _eT <- modifySimpleStore store (\x -> return (mod (x + 1) 10000))
                     _ <- createCheckpoint store
                     print x'
                     runMemTestLoop (eStore ,dir ,x' ,workingDir)

makeTestStores
  :: (Show c, Serialize c)
  => c
  -> IO
       (Either StoreError (SimpleStore c),
        Filesystem.Path.CurrentOS.FilePath, c,
        Filesystem.Path.CurrentOS.FilePath)
makeTestStores i = do 
   createDirectory True "xkcd-test-states"
   let dir = Filesystem.Path.concat ["xkcd-test-states" , decodeString.show $ i]
   workingDir <- getWorkingDirectory
   eStore <- makeSimpleStore dir i 
   return (eStore,dir,i,workingDir)

