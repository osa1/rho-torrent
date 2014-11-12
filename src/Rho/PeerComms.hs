{-# LANGUAGE OverloadedStrings #-}

-- | Communications with peers.
module Rho.PeerComms where

import           Control.Concurrent
import qualified Data.ByteString           as B
import qualified Data.ByteString.Builder   as BB
import qualified Data.ByteString.Char8     as BC
import qualified Data.ByteString.Lazy      as LB
import qualified Data.Map                  as M
import           Data.Monoid
import           Data.Time.Clock.POSIX     (POSIXTime, getPOSIXTime)
import           GHC.IO.Exception
import           Network.Socket            hiding (recv, recvFrom, send, sendTo)
import           Network.Socket.ByteString
import           System.IO.Error

data PeerConn = PeerConn
  { peerChoking    :: Bool
    -- ^ peer is choking us
  , peerInterested :: Bool
    -- ^ peer interested in something that we have to offer
  , choking        :: Bool
    -- ^ we're choking the peer
  , interested     :: Bool
    -- ^ we're interested in something that peer has to offer
  } deriving (Show)

data HandshakeStatus
    = HandshakeSent POSIXTime
    | HandshakeEstablished POSIXTime

initPeerCommsHandler :: IO Socket
initPeerCommsHandler = do
    sock <- socket AF_INET Stream defaultProtocol
    bind sock (SockAddrInet (fromIntegral (5433 :: Int)) 0)
    return sock

handshake
  :: SockAddr -> B.ByteString -> B.ByteString
  -> MVar (M.Map SockAddr HandshakeStatus)
  -> IO ()
handshake addr infoHash peerId hss = do
    peerStatus <- takeMVar hss
    ct <- getPOSIXTime
    case M.lookup addr peerStatus of
      Just (HandshakeSent t) -> do
        putStrLn $ "Handshake sent " ++ show (round $ ct - t) ++ " seconds ago. Skipping."
        putMVar hss peerStatus
      Just (HandshakeEstablished t) -> do
        putStrLn $ "Connection established " ++ show (round $ ct - t) ++ " seconds ago. Skipping."
        putMVar hss peerStatus
      Nothing -> do
        putStrLn $ "New peer. Initializing handshake."
        putMVar hss (M.insert addr (HandshakeSent ct) peerStatus)
        sendHandshake addr infoHash peerId

sendHandshake :: SockAddr -> B.ByteString -> B.ByteString -> IO ()
sendHandshake addr infoHash peerId = flip catchIOError errHandler $ do
    sock <- socket AF_INET Stream defaultProtocol
    bind sock (SockAddrInet aNY_PORT 0)
    putStrLn $ "Sending handshake to remote: " ++ show addr
    let msg = LB.toStrict . BB.toLazyByteString . mconcat $
                [ BB.word8 19 -- pstr len: standard for BitTorrent protocol
                , BB.byteString "BitTorrent protocol" -- pstr
                , BB.byteString $ B.pack [0, 0, 0, 0, 0, 0, 0, 0]
                , BB.byteString infoHash
                , BB.byteString peerId
                ]
    putStrLn $ "Handshake msg length: " ++ show (B.length msg)
    connect sock addr
    putStrLn "connected..."
    sent <- send sock msg
    -- I don't know how a peer is supposed to answer this, just try to read
    -- anything
    contents <- recv sock 1000
    putStrLn $ "Read contents: " ++ BC.unpack contents ++ " from: " ++ show addr
  where
    errHandler :: IOError -> IO ()
    errHandler err@IOError{ioe_type=NoSuchThing} = putStrLn $ "Problems with connection: " ++ show err
    errHandler err@IOError{ioe_type=TimeExpired} = putStrLn $ "Timeout happened: " ++ show err
    errHandler err = putStrLn $ "Unhandled error: " ++ show err
