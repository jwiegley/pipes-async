{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Main where

import           Control.Concurrent hiding (yield)
import           Control.Concurrent.STM
import           Control.Monad
import           Pipes
import           Pipes.Async
import qualified Pipes.Prelude as P
import           Pipes.Safe
import           Test.Hspec

segment1 :: Proxy x' x () Int (SafeT IO) ()
segment1 = do
    liftIO $ putStrLn "Starting segment 1"
    each [(1 :: Int)..3]

segment2 :: (Num b, MonadIO m) => TMVar String -> Proxy () b () b m ()
segment2 var = do
    liftIO $ putStrLn "Starting segment 2"
    replicateM_ 3 $ yield . (+1) =<< await
    -- P.map (+1)
    liftIO $ do
        putStrLn "atomically $ tryPutTMVar var \"sender\""
        atomically $ tryPutTMVar var "sender"
        putStrLn "Done sending"

segment3 :: (Num c, Show c, MonadIO m) => TMVar String -> Proxy () c () c m ()
segment3 var = do
    liftIO $ putStrLn "Starting segment 3"
    for cat $ \a -> do
        liftIO $ do
            putStrLn "Got a value from upstream"
            threadDelay 300000
            putStrLn "atomically $ tryPutTMVar var \"receiver\""
            atomically $ tryPutTMVar var "receiver"
            print a
        yield (a + 100)
    liftIO $ putStrLn "Done receiving"

segment4 :: Proxy () Int () Int (SafeT IO) b
segment4 = do
    liftIO $ putStrLn "Starting segment 4"
    P.map (+10)

main :: IO ()
main = hspec $ do
    describe "Sanity tests" $ do
        it "Works in the simplest case" $ do
            var' <- newEmptyTMVarIO
            ys <- runSafeT $ P.toListM $
                segment1 >-> (segment2 var' >-> segment3 var') >-> segment4
            ys `shouldBe` [112, 113, 114]

            var <- newEmptyTMVarIO
            xs <- runSafeT $ P.toListM $
                segment1 >-> (segment2 var >&> segment3 var) >-> segment4
            xs `shouldBe` [112, 113, 114]

            result <- atomically $ tryTakeTMVar var
            result `shouldBe` Just "sender"

        it "Behaves the same under composition (one)" $ do
            var' <- newEmptyTMVarIO
            ys <- runSafeT $ P.toListM $
                (segment1 >-> segment2 var') >-> segment3 var' >-> segment4
            ys `shouldBe` [112, 113, 114]

            var <- newEmptyTMVarIO
            xs <- runSafeT $ P.toListM $
                (segment1 >-> segment2 var) >&> segment3 var >-> segment4
            xs `shouldBe` [112, 113, 114]

            result <- atomically $ tryTakeTMVar var
            result `shouldBe` Just "sender"

        it "Behaves the same under composition (two)" $ do
            var' <- newEmptyTMVarIO
            ys <- runSafeT $ P.toListM $
                (segment1 >-> segment2 var' >-> segment3 var') >-> segment4
            ys `shouldBe` [112, 113, 114]

            var <- newEmptyTMVarIO
            xs <- runSafeT $ P.toListM $
                (segment1 >-> segment2 var >&> segment3 var) >-> segment4
            xs `shouldBe` [112, 113, 114]

            result <- atomically $ tryTakeTMVar var
            result `shouldBe` Just "sender"

        it "Behaves the same under composition (three)" $ do
            var' <- newEmptyTMVarIO
            ys <- runSafeT $ P.toListM $
                segment1 >-> segment2 var' >-> (segment3 var' >-> segment4)
            ys `shouldBe` [112, 113, 114]

            var <- newEmptyTMVarIO
            xs <- runSafeT $ P.toListM $
                segment1 >-> segment2 var >&> (segment3 var >-> segment4)
            xs `shouldBe` [112, 113, 114]

            result <- atomically $ tryTakeTMVar var
            result `shouldBe` Just "sender"

        it "Behaves the same under composition (four)" $ do
            var' <- newEmptyTMVarIO
            ys <- runSafeT $ P.toListM $
                segment1 >-> (segment2 var' >-> segment3 var' >-> segment4)
            ys `shouldBe` [112, 113, 114]

            var <- newEmptyTMVarIO
            xs <- runSafeT $ P.toListM $
                segment1 >-> (segment2 var >&> segment3 var >-> segment4)
            xs `shouldBe` [112, 113, 114]

            result <- atomically $ tryTakeTMVar var
            result `shouldBe` Just "sender"
