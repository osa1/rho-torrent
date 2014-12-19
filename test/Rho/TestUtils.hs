module Rho.TestUtils where

import           Control.Applicative
import           Control.Concurrent.Async
import           Control.DeepSeq
import qualified Control.Exception        as E
import qualified Data.ByteString          as B
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
