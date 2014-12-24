module Rho.TestUtils where

import           Control.Applicative
import           Control.Concurrent.Async
import           Control.DeepSeq
import qualified Control.Exception        as E
import qualified Data.ByteString          as B
import           Data.IORef
import           Data.List                (foldl')
import           Network.Socket

import           Test.HUnit.Lang
import           Test.QuickCheck

type Assertion' = IO

assertFailure' :: String -> Assertion' a
assertFailure' msg = msg `deepseq` E.throwIO (HUnitFailure msg)

genMaybe :: Gen a -> Gen (Maybe a)
genMaybe g = do
    b <- arbitrary
    if b then Just `fmap` g else return Nothing

ll :: [B.ByteString] -> Int
ll = foldl' (\acc b -> acc + B.length b) 0

-- | Create two sockets connected to each other.
initConnectedSocks :: IO (Socket, Socket)
initConnectedSocks = do
    localhost <- inet_addr "127.0.0.1"
    sock1 <- socket AF_INET Stream defaultProtocol
    bind sock1 (SockAddrInet aNY_PORT localhost)
    listen sock1 1
    sock1Port <- socketPort sock1
    connectedSock <- async (fst <$> accept sock1)

    sock2 <- socket AF_INET Stream defaultProtocol
    connect sock2 (SockAddrInet sock1Port localhost)

    sock1' <- wait connectedSock
    return (sock1', sock2)

-- | Create an IO action that returns next bytestring from the list in each
-- call.
mkMessageEmitter :: [B.ByteString] -> IO (IO B.ByteString)
mkMessageEmitter msgs = do
    msgsRef <- newIORef msgs
    return $ do
      ms <- readIORef msgsRef
      case ms of
        []       -> return B.empty -- signal closed channel
        (m : ms') -> do
          writeIORef msgsRef ms'
          return m

-- | Create an IO action that returns next byte from the bytestring in each
-- call.
mkByteEmitter :: B.ByteString -> IO (IO B.ByteString)
mkByteEmitter msg = do
    msgRef <- newIORef msg
    return $ do
      m <- readIORef msgRef
      case B.uncons m of
        Just (w, rest) -> do
          writeIORef msgRef rest
          return (B.singleton w)
        Nothing -> return B.empty -- signal closed socket

mkMessagePusher :: IO (B.ByteString -> IO (), IO B.ByteString)
mkMessagePusher = do
    ref <- newIORef []
    return (push ref, read_ ref)
  where
    push :: IORef [B.ByteString] -> B.ByteString -> IO ()
    push ref bs = atomicModifyIORef' ref $ \bss -> (bss ++ [bs], ())

    read_ :: IORef [B.ByteString] -> IO B.ByteString
    read_ ref =
      atomicModifyIORef' ref $ \bss ->
        case bss of
          []        -> ([], B.empty)
          (b : bs') -> (bs', b)
