module Rho.ListenerSpec where

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.MVar
import           Control.Monad
import qualified Data.ByteString          as B
import qualified Data.Dequeue             as D
import           Data.IORef
import           Data.List
import           Test.Hspec
import           Test.Hspec.HUnit
import           Test.Hspec.QuickCheck
import           Test.HUnit
import           Test.QuickCheck

import           Rho.Listener

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "ByteString Queue dequeueing" $
    modifyMaxSuccess (const 1000) $ prop "should be able to dequeue as long as len < queue len" $ do
      msgs <- genMsgs 100 20
      let deq = D.fromList msgs
          deqLen = foldl' (\acc b -> acc + B.length b) 0 msgs
      len <- oneof $ map return [0..deqLen - 1]
      let (_, msg) = dequeue deq len
      return $ B.length msg == len

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

    fromHUnitTest $ TestLabel "recvLen should keep returning empty after listener is stopped" $
      TestCase $ do
        emitter <- mkMessageEmitter []
        listener_ <- initListener emitter
        readMVar (stopped listener_)
        msgs <- mapM (recvLen listener_) [10, 20, 30]
        assertEqual "recvLen returned something wrong" msgs [B.empty, B.empty, B.empty]

    modifyMaxSuccess (const 100) $ prop "listener should be able to receive from emitter" $ do
      msgs <- genMsgs 100 20
      let msgsLen = ll msgs
      return $ ioProperty $ do
        emitter <- mkMessageEmitter msgs
        listener <- initListener emitter
        readMVar (stopped listener)
        -- The line below fails with "thread blocked indefinitely in an MVar operation"
        -- Try to guess why. `getChanContents` is awful.
        -- receivedMsg <- mconcat <$> getChanContents (buffer listener)
        receivedMsgs <- readBuffer listener
        return $ ll receivedMsgs == msgsLen

    modifyMaxSuccess (const 1000) $ prop "recvLen should be able to receive all messages" $ do
      msgs <- genMsgs 100 20
      let msgsLen = foldl' (\acc b -> acc + B.length b) 0 msgs
      recvLens <- generateRecvLens msgsLen
      return $ ioProperty $ do
        emitter <- mkMessageEmitter msgs
        listener <- initListener emitter
        rcvdMsgs <- mapM (recvLen listener) recvLens
        return $ all (\(rcvdLen, msg) -> rcvdLen == B.length msg) (zip recvLens rcvdMsgs)

ll :: [B.ByteString] -> Int
ll = foldl' (\acc b -> acc + B.length b) 0

genMsgs :: Int -> Int -> Gen [B.ByteString]
genMsgs maxMsgs maxMsgSize = do
    len <- oneof $ map return [1..maxMsgs]
    replicateM len (genMsg maxMsgSize)

genMsg :: Int -> Gen B.ByteString
genMsg maxMsgSize = do
    len <- oneof $ map return [1..maxMsgSize]
    B.pack <$> replicateM len arbitrary

mkMessageEmitter :: [B.ByteString] -> IO (IO B.ByteString)
mkMessageEmitter msgs = do
    msgsRef <- newIORef msgs
    return $ do
      ms <- readIORef msgsRef
      case ms of
        []       -> return B.empty -- signal closed channel
        (m : ms) -> do
          writeIORef msgsRef ms
          return m

-- | Generate list of ints with given sum.
generateRecvLens :: Int -> Gen [Int]
generateRecvLens 0 = return []
generateRecvLens n = do
    i <- oneof $ map return [0..n]
    r <- generateRecvLens (n - i)
    return $ i : r

readBuffer :: Listener -> IO [B.ByteString]
readBuffer l@Listener{deque=deq} = do
    (d, _) <- readIORef deq
    return $ D.takeFront (D.length d) d
