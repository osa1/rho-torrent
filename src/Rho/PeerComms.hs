{-# LANGUAGE NondecreasingIndentation, OverloadedStrings #-}

-- | Communications with peers.
module Rho.PeerComms where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad
import qualified Data.ByteString           as B
import qualified Data.Map                  as M
import           GHC.IO.Exception
import           Network.Socket            hiding (KeepAlive, recv, recvFrom,
                                            send, sendTo)
import           Network.Socket.ByteString
import           System.IO.Error

import           Rho.InfoHash
import           Rho.PeerComms.Handshake
import           Rho.PeerComms.Message

data PeerConn = PeerConn
  { pcPeerChoking    :: Bool
    -- ^ peer is choking us
  , pcPeerInterested :: Bool
    -- ^ peer interested in something that we have to offer
  , pcChoking        :: Bool
    -- ^ we're choking the peer
  , pcInterested     :: Bool
    -- ^ we're interested in something that peer has to offer
  , pcPeerId         :: PeerId
  , pcOffers         :: [InfoHash]
    -- ^ torrents that the peer offers
  , pcSock           :: Socket
    -- ^ socket connected to the peer
  , pcExtendedMsgTbl :: ExtendedPeerMsgTable
  } deriving (Show)

-- | Stat of peers, shared between workers.
type PeersState = MVar (M.Map SockAddr PeerConn)

data PeerCommHandler = PeerCommHandler
  { pchPeers :: PeersState }

-- | Initialize listeners, data structures etc. for peer communications.
initPeerCommsHandler :: IO PeerCommHandler
initPeerCommsHandler = do
    -- state of all connected peers
    peers <- newMVar M.empty
    -- sock for incoming connections
    sock <- socket AF_INET Stream defaultProtocol
    bind sock (SockAddrInet (fromIntegral (5433 :: Int)) 0) -- TODO: hard-coded port
    -- We don't block the thread that listens for incoming handshakes
    void $ async $ listenPeerSocket sock peers
    return $ PeerCommHandler peers

-- | Accept incoming connections and spawn a connected socket listener for
-- every accepted connection.
listenPeerSocket :: Socket -> PeersState -> IO ()
listenPeerSocket sock peers = do
    (peerSock, peerAddr) <- accept sock
    -- have we established a connection with the peer?
    peers' <- readMVar peers
    if M.member peerAddr peers'
      then
        void $ async $ listenConnectedSock peerSock peerAddr peers
      else
        -- we should have a handshake message as first thing
        void $ async $ listenHandshake peerSock peerAddr peers
    listenPeerSocket sock peers

-- | Wait for an incoming handshake, update peers state upon successfully
-- parsing the handshake and continue listening the connected socket.
listenHandshake :: Socket -> SockAddr -> PeersState -> IO ()
listenHandshake peerSock _peerAddr _peers = void $ async $ do
    msg <- recv peerSock 1000
    case parseHandshake msg of
      Left err -> putStrLn $ "Can't parse handshake: " ++ err
      Right (_infoHash, _peerId, _extra) -> do
        -- TODO: we don't seed yet
        putStrLn "Ignoring an incoming handshake."

-- | Listen a connected socket and handle incoming messages.
listenConnectedSock :: Socket -> SockAddr -> PeersState -> IO ()
listenConnectedSock sock sockAddr peers = flip catchIOError errHandler $ do
    msg <- recv sock 10000
    -- empty message == connection closed
    if B.null msg
      then do
        putStrLn $ "Connection closed with peer: " ++ show sockAddr
        closeConn
      else do
        handleMessage msg sock sockAddr peers
        listenConnectedSock sock sockAddr peers
  where
    errHandler err = do
      putStrLn $ "Error happened while listening a socket: " ++ show err
                  ++ ". Closing the connection."
      closeConn

    closeConn = modifyMVar_ peers $ return . M.delete sockAddr

handleMessage :: B.ByteString -> Socket -> SockAddr -> PeersState -> IO ()
handleMessage msg _sock _sockAddr _peers = do
    case parsePeerMsg msg of
      Left err -> putStrLn $ "Can't parse peer message: " ++ err
      Right pmsg -> putStrLn $ "Parsed peer msg: " ++ show pmsg

handshake :: PeerCommHandler -> SockAddr -> InfoHash -> PeerId -> IO ()
handshake PeerCommHandler{pchPeers=peers} addr infoHash peerId = do
    ret <- sendHandshake addr infoHash peerId
    case ret of
      Left err -> putStrLn $ "Can't establish connection: " ++ err
      Right (sock, infoHash', peerId', extra) -> do
        putStrLn "Handshake successful"
        peers' <- takeMVar peers
        case M.lookup addr peers' of
          Nothing -> do
            -- first time handshaking with the peer, spawn a socket
            -- listener
            void $ async $ listenConnectedSock sock addr peers
            -- TODO: check info_hash
            let peerConn = PeerConn True False True False peerId [infoHash] sock M.empty
            putMVar peers $ M.insert addr peerConn peers'
          Just pc -> do
            -- probably learned about a new torrent
            -- since we already knew about this peer, there should be
            -- a thread listening for messages from this socket. no need
            -- to create a new one.
            putMVar peers $ M.insert addr pc{pcOffers = infoHash : pcOffers pc} peers'
            -- handle extra message
            handleMessage extra (pcSock pc) addr peers

-- | Send a handshake message to given target using a fresh socket. Return
-- the connected socket in case of a success. (e.g. receiving answer to
-- handshake)
sendHandshake
    :: SockAddr -> InfoHash -> PeerId
    -> IO (Either String (Socket, InfoHash, PeerId, B.ByteString))
sendHandshake addr infoHash peerId = flip catchIOError errHandler $ do
    sock <- socket AF_INET Stream defaultProtocol
    bind sock (SockAddrInet aNY_PORT 0)
    putStrLn $ "Sending handshake to remote: " ++ show addr
    let msg = mkHandshake infoHash peerId
    connect sock addr
    sent <- send sock msg
    bytes <- recv sock 10000
    case parseHandshake bytes of
      Left err -> return $ Left err
      Right (infoHash', peerId', extra) -> return $ Right (sock, infoHash', peerId', extra)
  where
    errHandler err@IOError{ioe_type=NoSuchThing} =
      return $ Left $ "Problems with connection: " ++ show err
    errHandler err@IOError{ioe_type=TimeExpired} =
      return $ Left $ "Timeout happened: " ++ show err
    errHandler err =
      return $ Left $ "Unhandled error: " ++ show err
