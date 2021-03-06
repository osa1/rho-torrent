{-# LANGUAGE OverloadedStrings #-}

module Rho.ListenerSpec where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad
import qualified Data.ByteString          as B
import qualified Data.ByteString.Lazy     as LB
import           Data.IORef

import           Test.Hspec
import           Test.Hspec.Contrib.HUnit
import           Test.Hspec.QuickCheck
import           Test.HUnit
import           Test.QuickCheck

import           Rho.Listener
import           Rho.TestUtils
import           Rho.Utils

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Listeners" $ do
    fromHUnitTest $ TestLabel "listener thread should stop when emitter is closed" $
      TestCase $ do
        emitter <- mkMessageEmitter []
        listener_ <- initListener emitter
        readMVar (stopped listener_)
        -- allow listener to return after `putMVar stopped ()`
        threadDelay 100
        ret <- poll (listener listener_)
        case ret of
          Nothing -> assertFailure "Listener is not stopped after messages consumed."
          Just (Right ()) -> return ()
          Just (Left _) -> assertFailure "Listener is failed with exception."

    fromHUnitTest $ TestLabel
      "recvLen should return whatever left in the buffer after listener is stopped" $
        TestCase $ do
          let msgs = map B.pack [[1, 2, 3, 4], [5, 6]]
          emitter <- mkMessageEmitter msgs
          listener_ <- initListener emitter
          msg <- recvLen listener_ (ll msgs + 1)
          assertEqual "recvLen did not return all it read" (ll msgs) (B.length msg)
          msg' <- recvLen listener_ 10
          assertEqual "recvLen did not return empty after buffer is cleared" 0 (B.length msg')
          buf <- readIORef (buffer listener_)
          assertBool "buffer not empty" (LB.null buf)
          dld <- readIORef $ totalDownloaded listener_
          assertEqual "totalDownloaded is wrong" (ll msgs) dld

    fromHUnitTest $ TestLabel "recvLen should keep returning empty after listener is stopped" $
      TestCase $ do
        emitter <- mkMessageEmitter []
        listener_ <- initListener emitter
        readMVar (stopped listener_)
        msgs <- mapM (recvLen listener_) [10, 20, 30]
        assertEqual "recvLen returned something wrong" msgs [B.empty, B.empty, B.empty]
        buf <- readIORef (buffer listener_)
        assertBool "buffer not empty" (LB.null buf)
        dld <- readIORef $ totalDownloaded listener_
        assertEqual "totalDownloaded is wrong" 0 dld

    fromHUnitTest $ TestLabel "recvLen should not receive after listener is manually stopped" $
      TestCase $ do
        (push, recv) <- mkMessagePusher
        listener_ <- initListener recv
        let msg1 = "message 1"
            msg2 = "message 2"
        push msg1
        yield
        msg1' <- recvLen listener_ 9
        assertEqual "received message is wrong" msg1 msg1'
        stopListener listener_
        msg2' <- recvLen listener_ 9
        assertEqual "received message is wrong" B.empty msg2'
        push msg2
        msg2'' <- recvLen listener_ 9
        assertEqual "received message is wrong" B.empty msg2''
        dld <- readIORef $ totalDownloaded listener_
        assertEqual "totalDownloaded is wrong" (B.length msg1) dld

    fromHUnitTest $ TestLabel "sending bytes one-by-one" $
      TestCase $ do
        let bytes = replicate 100 (B.singleton 0x12)
        emitter <- mkMessageEmitter bytes
        listener_ <- initListener emitter
        msg <- recvLen listener_ 100
        assertEqual "recvLen returned something wrong" msg (mconcat bytes)
        buf <- readIORef (buffer listener_)
        assertBool "buffer not empty" (LB.null buf)
        dld <- readIORef $ totalDownloaded listener_
        assertEqual "totalDownloaded is wrong" (ll bytes) dld

    fromHUnitTest $ TestLabel "recvLen bug" $ TestCase $ do
      let msgs = map B.pack [[0,0,0,1,1], [0,0,0,0]]
      emitter <- mkMessageEmitter msgs
      listener_ <- initListener emitter
      msg1 <- recvLen listener_ 5
      msg2 <- recvLen listener_ 4
      assertEqual "first message is wrong" [0,0,0,1,1] (B.unpack msg1)
      assertEqual "second message is wrong" [0,0,0,0] (B.unpack msg2)
      buf <- readIORef (buffer listener_)
      assertBool "buffer not empty" (LB.null buf)

    modifyMaxSuccess (const 100) $ prop "listener should be able to receive from emitter" $ do
      msgs <- genMsgs 100 20
      let msgsLen = ll msgs
      return $ ioProperty $ do
        emitter <- mkMessageEmitter msgs
        listener_ <- initListener emitter
        readMVar (stopped listener_)
        -- The line below fails with "thread blocked indefinitely in an MVar operation"
        -- Try to guess why. `getChanContents` is awful.
        -- receivedMsg <- mconcat <$> getChanContents (buffer listener)
        receivedMsgs <- readIORef $ buffer listener_
        return $ fromIntegral (LB.length receivedMsgs) == msgsLen

    modifyMaxSuccess (const 100) $ prop "recvLen should be able to receive all messages" $ do
      msgs <- genMsgs 100 20
      let msgsLen = ll msgs
      recvLens <- generateRecvLens msgsLen
      return $ ioProperty $ do
        emitter <- mkMessageEmitter msgs
        listener_ <- initListener emitter
        rcvdMsgs <- mapM (recvLen listener_) recvLens
        return $ all (\(rcvdLen, msg) -> rcvdLen == B.length msg) (zip recvLens rcvdMsgs)

    fromHUnitTest $ TestLabel "download speed should init with 0" $ TestCase $ do
      emitter <- mkMessageEmitter []
      listener_ <- initListener emitter
      s <- downloadSpeed listener_
      assertEqual "download speed is not 0" 0 s

    fromHUnitTest $ TestLabel "download speed should reset after 0.1 seconds" $ TestCase $ do
      let msg = "message"
      emitter <- mkMessageEmitter [msg]
      let dt = 100
      listener_ <- initListener' emitter dt
      threadDelay 1000
      s1 <- downloadSpeed listener_
      assertEqual "download speed is wrong" (fromIntegral (B.length msg) / fromIntegral dt) s1
      threadDelay 100000
      s2 <- downloadSpeed listener_
      assertEqual "download speed is wrong" 0 s2

    modifyMaxSuccess (const 10) $ prop "random download speed tests" $ do
      msgs <- genMsgs 10 10
      let msgsLen = ll msgs
      return $ ioProperty $ do
        emitter <- mkMessageEmitter msgs
        listener_ <- initListener emitter
        threadDelay 1000
        s <- downloadSpeed listener_
        return $ s == fromIntegral msgsLen / (5 * 1000)

    fromHUnitTest $ TestLabel "exc safety: async exc while blocked in `recv` should release locks" $
      TestCase $ do
        listener_ <- initListener blockingIOAction
        threadDelay 10000
        cancel $ listener listener_
        threadDelay 10000
        checkLocks listener_

    fromHUnitTest $ TestLabel "exc safety: sync exc thrown by `recv` should release locks" $
      TestCase $ do
        listener_ <- initListener failingIOAction
        threadDelay 10000
        checkLocks listener_

checkLocks :: Listener -> Assertion
checkLocks listener_ = do
    stopped_ <- not <.> isEmptyMVar $ stopped listener_
    updated_ <- not <.> isEmptyMVar $ updated listener_
    unlocked <- not <.> isEmptyMVar $ lock listener_
    assertBool "stopped is not signalled" stopped_
    assertBool "updated is not signalled" updated_
    assertBool "lock is not free"  unlocked

genMsgs :: Int -> Int -> Gen [B.ByteString]
genMsgs maxMsgs maxMsgSize = do
    len <- oneof $ map return [1..maxMsgs]
    replicateM len (genMsg maxMsgSize)

genMsg :: Int -> Gen B.ByteString
genMsg maxMsgSize = do
    len <- oneof $ map return [1..maxMsgSize]
    B.pack <$> replicateM len arbitrary

-- | Generate list of ints with given sum.
generateRecvLens :: Int -> Gen [Int]
generateRecvLens 0 = return []
generateRecvLens n = do
    i <- oneof $ map return [0..n]
    r <- generateRecvLens (n - i)
    return $ i : r
