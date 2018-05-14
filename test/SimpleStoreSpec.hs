{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
module SimpleStoreSpec (main
                       , spec
                       , makeTestStore) where

import           Control.Applicative
import           Control.Concurrent        (threadDelay,forkIO)
import           Control.Monad (void)
import           Control.Concurrent.Async
import           Control.Concurrent.STM
-- import           Control.Concurrent.STM.TMVar
import           Data.Either
import           Data.Traversable
-- import           Data.Traversable
import qualified Data.ByteString           as BS
import           Data.Function
import           Data.List                 (sortBy)
import qualified Data.Serialize            as S
import qualified Data.Text                 as Text
import qualified Data.Text.IO              as Text
import           Filesystem
import           Filesystem.Path
import           Filesystem.Path.CurrentOS (encodeString)
import           Prelude                   hiding (sequence)
import           SimpleStore
import           Test.Hspec



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




-- | Purposefully corrupt the file last touched by the system
corruptOneState :: IO ()
corruptOneState = do
  let dir = "test-states"
  lst <- listDirectory dir
  let (fp:_) = sortBy (compare `on` lexicalFirstChar) $
                     filter (\fp' -> fp' /= "test-states/open.lock" ) $ lst

      lexicalFirstChar j = Prelude.take 1 . encodeString $ j

  putStrLn (show fp)
  val <-  Text.readFile "test-states/last.touch"
  let corruption_string = BS.pack [1,2,3,4]
  BS.writeFile (Text.unpack val) corruption_string


-- | Purposefully corrupt the file last touched by the system
emptyOneState :: IO ()
emptyOneState = do
  let dir = "test-states"
  lst <- listDirectory dir
  let (fp:_) = sortBy (compare `on` lexicalFirstChar) $
                     filter (\fp' -> fp' /= "test-states/open.lock" ) $ lst

      lexicalFirstChar j = Prelude.take 1 . encodeString $ j

  putStrLn (show fp)
  val <-  Text.readFile "test-states/last.touch"
  let corruption_string = BS.pack []
  BS.writeFile (Text.unpack val) corruption_string

makeTestStore  :: IO (Either StoreError (SimpleStore Int),
                   Filesystem.Path.FilePath,
                   Int,
                   Filesystem.Path.FilePath)
makeTestStore = do
   let x = 10 :: Int
       dir = "test-states"
   workingDir <- getWorkingDirectory
   eStore <- makeSimpleStore dir x
   return (eStore,dir,x,workingDir)


forkCreateCheckpoints  :: (Traversable t, S.Serialize st) =>  t (SimpleStore st) -> IO ()
forkCreateCheckpoints eStore = do
  _ <- forkIO $ void $  sequence $ createCheckpoint <$> eStore
  _ <- forkIO $ void $  sequence $ createCheckpointImmediate <$> eStore
  _ <- forkIO $ void $  sequence $ createCheckpointImmediate <$> eStore
  _ <- forkIO $ void $  sequence $ createCheckpointImmediate <$> eStore
  _ <- forkIO $ void $  sequence $ createCheckpointImmediate <$> eStore
  _ <- forkIO $ void $  sequence $ createCheckpointImmediate <$> eStore
  _ <- forkIO $ void $  sequence $ createCheckpointImmediate <$> eStore
  _ <- forkIO $ void $  sequence $ createCheckpointImmediate <$> eStore
  _ <- forkIO $ void $  sequence $ createCheckpointImmediate <$> eStore
  _ <- forkIO $ void $  sequence $ createCheckpointImmediate <$> eStore
  _ <- forkIO $ void $  sequence $ createCheckpointImmediate <$> eStore
  _ <- forkIO $ void $  sequence $ createCheckpointImmediate <$> eStore
  _ <- forkIO $ void $  sequence $ createCheckpointImmediate <$> eStore
  _ <- forkIO $ void $  sequence $ createCheckpoint <$> eStore
  return ()
spec :: Spec
spec = do

  describe "Making, creating checkpoints, closing, reopening" $ do
    it "should open an initial state, create checkpoints, and then open the state back up" $ do
      r <- isDirectory "test-states"
      if r
         then removeTree "test-states"
         else return ()

      -- let x = 10 :: Int
      --     dir = "test-states"
      -- workingDir <- getWorkingDirectory
      -- eStore <- makeSimpleStore dir x
      (eStore,dir,x,workingDir)  <- makeTestStore
      _ <- forkIO $ void $ sequence $ getSimpleStore   <$> eStore
      _ <- forkIO $ void $  sequence $ createCheckpoint <$> eStore
      _ <- forkIO $ void $  sequence $ createCheckpoint <$> eStore
      _ <- forkIO $ void $  sequence $ createCheckpoint <$> eStore
      _ <- forkIO $ void $  sequence $ createCheckpoint <$> eStore
      _ <- forkIO $ void $  sequence $ createCheckpoint <$> eStore
      _ <- forkIO $ void $  sequence $ createCheckpoint <$> eStore
      _ <- forkIO $ void $  sequence $ closeSimpleStore <$> eStore
      eStore' <- openSimpleStore dir
      case eStore' of
        (Left err) -> fail ("Unable to open local state" ++ show err)
        (Right store) -> do
          x' <- getSimpleStore store
          removeTree $ workingDir </> dir
          _ <- closeSimpleStore store
          x `shouldBe` x'
  describe "Make, close, open, modify, close open, check value" $ do
    it "Should create a new state, close it, then open the state and modify the state, close the state, and finally open it and check the value" $ do
      let initial = 100 :: Int
          modifyX x = x + 1000
          dir = "test-states"
      workingDir <- getWorkingDirectory
      (Right store) <- makeSimpleStore dir initial
      _ <- createCheckpoint store
      closeSimpleStore store
      (Right store') <- openSimpleStore dir :: IO (Either StoreError (SimpleStore Int))
      _ <- createCheckpoint store'
      _ <- (\lStore -> modifySimpleStore lStore (return . modifyX)) store'
      _ <- createCheckpoint store'
      closeSimpleStore store'
      eStore'' <- openSimpleStore dir :: IO (Either StoreError (SimpleStore Int))
      case eStore'' of
        (Left err) -> fail ("Unable to open local state" ++ show err)
        (Right _) -> do
          x' <- getSimpleStore store'
          removeTree (workingDir </> dir)
          _ <- closeSimpleStore `traverse` eStore''
          x' `shouldBe` (modifyX initial)
  describe "Async updating/creating checkpoints for a state" $ do
    it "Should start 100 threads trying to update a state and should modify the state correctly, be able to close and reopen the state, and then read the correct value" $ do
      let
        initial = 0 :: Int
        initialString = show initial
        modifyX = (+ (2::Integer))
        dir = "test-states"
        functions = replicate 100  (\tv x -> (atomically $ readTMVar tv) >>
                                             (return . show . modifyX . read $ x)         )


      waitTVar <- newEmptyTMVarIO
      (Right store) <- makeSimpleStore dir initialString

      _ <- createCheckpoint store


      aRes <- traverse (\func -> async $ do
                          _ <- modifySimpleStore store (func waitTVar)
                          createCheckpoint store) functions :: IO [Async (Either StoreError ())]

      atomically $ putTMVar waitTVar ()
      _ <- traverse wait aRes

      x'' <- getSimpleStore store
      putStrLn $ "x -> " ++ (show x'')
      _ <- createCheckpoint store
      closeSimpleStore store

      eStore <- openSimpleStore dir
      let store' = either (error . show) id eStore
      x' <- getSimpleStore store'
      _ <- putStrLn $ "x' -> " ++ (show x')
      _ <- closeSimpleStore `traverse` eStore
      x' `shouldBe` ("200" :: String)
  describe "test ordered state file" $ do
    it "should make sure the right file is being opened" $ do
      let dir = "test-states"
      eStore  <- openSimpleStore dir :: IO (Either StoreError (SimpleStore Int))
      _       <- getSimpleStore `traverse` eStore
      _       <- (flip modifySimpleStore (return . (1 +)) ) `traverse` eStore
      _       <- createCheckpointImmediate `traverse` eStore
      ey      <- getSimpleStore `traverse` eStore
      _       <- closeSimpleStore `traverse` eStore
      eStore' <- openSimpleStore dir :: IO (Either StoreError (SimpleStore Int))
      ex'     <- getSimpleStore `traverse` eStore'
      _       <- closeSimpleStore `traverse` eStore'
      _       <- putStrLn (show ex')
      (isRight' ex') `shouldBe` (Right True)
      ex' `shouldBe` ey


  describe "blank store file should open something (though may not be correct)" $ do
    it "Should corrupt a file and then open an older file (with possible wrong state)" $ do
          let dir = "test-states"
          eStore <- openSimpleStore dir :: IO (Either StoreError (SimpleStore (BoundInt)))
          _ex <- getSimpleStore `traverse` eStore
          _ <- (flip modifySimpleStore (return . (1 +)) ) `traverse` eStore
          _ <- threadDelay (1 * 10^(6::Integer))
          _ <- createCheckpointImmediate `traverse` eStore
          _ <- forkCreateCheckpoints eStore
          _ <- closeSimpleStore `traverse` eStore
          putStrLn "empty"
          _ <- emptyOneState
          putStrLn "done"
          eStore' <- openSimpleStore dir :: IO (Either StoreError (SimpleStore BoundInt))
          ex'     <- getSimpleStore   `traverse` eStore'
          _       <- closeSimpleStore `traverse` eStore'
          _       <- putStrLn (show ex')

          ex'           `shouldBe` ex'
          (isRight' ex') `shouldBe` (Right True)

  describe "purposefully corrupt file" $ do
    it "Should corrupt a file and then open the old file and read it" $ do
          let dir = "test-states"
          eStore <- openSimpleStore dir :: IO (Either StoreError (SimpleStore (BoundInt)))
          ex <- getSimpleStore `traverse` eStore
          _ <- (flip modifySimpleStore (return . (const 1)) ) `traverse` eStore
          _ <- (flip modifySimpleStore (return . (1 +)) ) `traverse` eStore
          _ <- threadDelay (1 * 10^(6::Integer))
          _ <- createCheckpoint `traverse` eStore
          _ <- closeSimpleStore `traverse` eStore
          putStrLn "corrupt"
          _ <- corruptOneState
          putStrLn "done"
          eStore' <- openSimpleStore dir :: IO (Either StoreError (SimpleStore BoundInt))
          ex'     <- getSimpleStore   `traverse` eStore'
          _       <- closeSimpleStore `traverse` eStore'
          _       <- putStrLn (show ex')

          ex'           `shouldBe` ex
          (isRight' ex') `shouldBe` (Right True)



-- Helper function for test conditions

isRight' :: Either a t -> Either a Bool
isRight' (Right _ )  = Right True
isRight' (Left  s )  = Left s
