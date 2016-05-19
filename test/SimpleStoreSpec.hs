{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module SimpleStoreSpec (main
                       , spec
                       , makeTestStore) where

import           Control.Applicative
-- import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM
-- import           Control.Concurrent.STM.TMVar
import           Data.Either
import           Data.Traversable
-- import           Data.Traversable
import           Filesystem
import           Filesystem.Path
import Filesystem.Path.CurrentOS (encodeString)  
import           Prelude                      hiding (sequence)
import           SimpleStore
import           Test.Hspec
import Data.List (sort,filter,reverse,sortBy)
import qualified System.IO  as System
import Data.Function 

import qualified Data.Serialize               as S



-- | Bound int allows failure when reading back corrupted data
newtype BoundInt = BoundInt Int
 deriving (Num ,Eq,Show,Ord)


instance S.Serialize BoundInt where
  put (BoundInt i) = S.put i
  get = S.get >>= failOnBig
    where
      failOnBig i
        | i > 10000 = fail "int too big to fail "
        | otherwise = return $ BoundInt i


main :: IO ()
main = do  
  hspec spec

corruptOneState :: IO ()
corruptOneState = do
  let dir = "test-states"
  lst <- listDirectory dir
  putStrLn (show lst)
  let (fp:sorted) = sortBy (compare `on` lexicalFirstChar) $
                    filter (\fp -> fp /= "test-states/open.lock" ) $ lst
                    
      lexicalFirstChar j = Prelude.take 1 . encodeString $ j
      
  putStrLn (show fp)
  System.writeFile (encodeString .  handleFpAt4 $ fp) "corrupt on purpose"
 where
   handleFpAt4 fp = if fp == "test-states/4checkpoint.st"
                       then "test-states/0checkpoint.st"
                       else fp

makeTestStore = do 
   let x = 10 :: Int
       dir = "test-states"
   workingDir <- getWorkingDirectory
   eStore <- makeSimpleStore dir x 
   return (eStore,dir,x,workingDir)


makeTestTextStore = do 
   let x = "10" :: String
       dir = "test-states"
   workingDir <- getWorkingDirectory
   eStore <- makeSimpleStore dir x 
   return (eStore,dir,x,workingDir)

spec :: Spec
spec = do
  
  describe "Making, creating checkpoints, closing, reopening" $ do    
    it "should open an initial state, create checkpoints, and then open the state back up" $ do
      removeTree "test-states"
      -- let x = 10 :: Int
      --     dir = "test-states"
      -- workingDir <- getWorkingDirectory
      -- eStore <- makeSimpleStore dir x
      (eStore,dir,x,workingDir)  <- makeTestStore
      sequence $ getSimpleStore   <$> eStore
      sequence $ createCheckpoint <$> eStore
      sequence $ createCheckpoint <$> eStore
      sequence $ createCheckpoint <$> eStore
      sequence $ createCheckpoint <$> eStore
      sequence $ createCheckpoint <$> eStore
      sequence $ createCheckpoint <$> eStore
      sequence $ closeSimpleStore <$> eStore
      eStore' <- openSimpleStore dir
      case eStore' of
        (Left err) -> fail "Unable to open local state"
        (Right store) -> do
          x' <- getSimpleStore store
          removeTree $ workingDir </> dir
          x `shouldBe` x'
  describe "Make, close, open, modify, close open, check value" $ do
    it "Should create a new state, close it, then open the state and modify the state, close the state, and finally open it and check the value" $ do
      let initial = 100 :: Int
          modifyX x = x + 1000
          dir = "test-states"
      workingDir <- getWorkingDirectory
      (Right store) <- makeSimpleStore dir initial
      createCheckpoint store
      closeSimpleStore store
      (Right store') <- openSimpleStore dir :: IO (Either StoreError (SimpleStore Int))
      createCheckpoint store'
      (\store -> modifySimpleStore store (return . modifyX)) store'
      createCheckpoint store'
      closeSimpleStore store'
      eStore'' <- openSimpleStore dir :: IO (Either StoreError (SimpleStore Int))
      case eStore'' of
        (Left err) -> fail "Unable to open local state"
        (Right store) -> do
          x' <- getSimpleStore store'
          removeTree $ workingDir </> dir
          closeSimpleStore `traverse` eStore''
          x' `shouldBe` (modifyX initial)
  describe "Async updating/creating checkpoints for a state" $ do
    it "Should start 100 threads trying to update a state and should modify the state correctly, be able to close and reopen the state, and then read the correct value" $ do
      let initial = 0 :: Int
          modifyX = (+2)
          dir = "test-states"
          functions = replicate 100  (\tv x -> (atomically $ readTMVar tv) >> (return . modifyX $ x))
      waitTVar <- newEmptyTMVarIO
      (Right store) <- makeSimpleStore dir initial
      
      createCheckpoint store
      aRes <- traverse (\func -> async $ do
                          modifySimpleStore store (func waitTVar)
                          createCheckpoint store) functions
      atomically $ putTMVar waitTVar ()
      results <- traverse wait aRes
      x'' <- getSimpleStore store
      putStrLn $ "x -> " ++ (show x'')
      createCheckpoint store
      closeSimpleStore store
      eStore <- openSimpleStore dir
      let store' = either (error . show) id eStore
      x' <- getSimpleStore store'
      closeSimpleStore `traverse` eStore
      x' `shouldBe` (200 :: Int)
  describe "test ordered state file" $ do
    it "should make sure the right file is being opened" $ do
      let dir = "test-states"
      eStore <- openSimpleStore dir :: IO (Either StoreError (SimpleStore Int))          
      ex <- getSimpleStore `traverse` eStore
      _ <- (flip modifySimpleStore (return . (1 +)) ) `traverse` eStore
      _ <- createCheckpoint `traverse` eStore
      ey <- getSimpleStore `traverse` eStore      
      _ <- closeSimpleStore `traverse` eStore
      eStore' <- openSimpleStore dir :: IO (Either StoreError (SimpleStore Int))          
      ex' <- getSimpleStore `traverse` eStore'          
      closeSimpleStore `traverse` eStore'
      putStrLn (show ex')
      (isRight ex') `shouldBe` (Right True) 
      ex' `shouldBe` ey

  describe "purposefully corrupt file" $ do
    it "Should corrupt a file and then open the old file and read it" $ do    
          let dir = "test-states"          
          eStore <- openSimpleStore dir :: IO (Either StoreError (SimpleStore (BoundInt)))          
          ex <- getSimpleStore `traverse` eStore
          _ <- (flip modifySimpleStore (return . (1 +)) ) `traverse` eStore
          _ <- createCheckpoint `traverse` eStore
          _ <- closeSimpleStore `traverse` eStore
          _ <- corruptOneState
          eStore' <- openSimpleStore dir :: IO (Either StoreError (SimpleStore BoundInt))          
          ex' <- getSimpleStore `traverse` eStore'          
          closeSimpleStore `traverse` eStore'
          putStrLn (show ex')
          ex' `shouldBe` ex
          (isRight ex') `shouldBe` (Right True) 
            where
              isRight (Right _) = Right True
              isRight (Left s)  = Left s





