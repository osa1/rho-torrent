{-# LANGUAGE NondecreasingIndentation, OverloadedStrings #-}

-- | Communications with peers.
module Rho.PeerComms where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad
import qualified Data.ByteString           as B
import qualified Data.Map                  as M
import           Data.Maybe
import           GHC.IO.Exception
import           Network.Socket            hiding (KeepAlive, recv, recvFrom,
                                            send, sendTo)
import           Network.Socket.ByteString
import           System.IO.Error
import qualified System.Log.Logger         as L

import qualified Rho.Bitfield              as BF
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
  , pcOffers         :: InfoHash
    -- ^ torrent that the peer offers
  , pcPieces         :: Maybe BF.Bitfield
    -- TODO: remove Maybe and initialize with empty bitfield
  , pcSock           :: Socket
    -- ^ socket connected to the peer
  , pcExtendedMsgTbl :: ExtendedPeerMsgTable
  } deriving (Show)

newPeerConn :: PeerId -> InfoHash -> Socket -> PeerConn
newPeerConn peerId infoHash sock = PeerConn True False True False peerId infoHash Nothing sock M.empty

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
    unless (B.null msg) $
      case parseHandshake msg of
        Left err ->
          warning $ "Can't parse handshake: " ++ err ++ " msg: " ++ show (B.unpack msg)
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
handleMessage msg _sock peerAddr peers = do
    case parsePeerMsg msg of
      Left err -> warning $ "Can't parse peer message: " ++ err ++ " msg: " ++ show (B.unpack msg)
      Right KeepAlive -> return () -- TODO: should I ignore keep-alives?
      Right (Bitfield bf) -> modifyPeerState $ \pc -> pc{pcPieces = Just bf}
      Right (Have piece) ->
        modifyPeerState $ \pc ->
          let bf' = Just $ BF.set (fromMaybe BF.empty $ pcPieces pc) (fromIntegral piece)
          in pc{pcPieces = bf'}
      Right Choke -> modifyPeerState $ \pc -> pc{pcPeerChoking = True}
      Right Unchoke -> modifyPeerState $ \pc -> pc{pcPeerChoking = False}
      Right Interested -> modifyPeerState $ \pc -> pc{pcPeerInterested = True}
      Right NotInterested -> modifyPeerState $ \pc -> pc{pcPeerInterested = False}
      Right pmsg -> putStrLn $ "Unhandled peer msg: " ++ show pmsg
  where
    modifyPeerState :: (PeerConn -> PeerConn) -> IO ()
    modifyPeerState m = withPeer $ \peers' pc -> return (M.insert peerAddr (m pc) peers')

    withPeer :: (M.Map SockAddr PeerConn -> PeerConn -> IO (M.Map SockAddr PeerConn)) -> IO ()
    withPeer f =
      modifyMVar_ peers $ \peers' ->
        case M.lookup peerAddr peers' of
          Nothing -> errorLog "Can't find peer in peers state" >> return peers'
          Just pc -> f peers' pc

handshake :: PeerCommHandler -> SockAddr -> InfoHash -> PeerId -> IO ()
handshake PeerCommHandler{pchPeers=peers} addr infoHash peerId = do
    ret <- sendHandshake addr infoHash peerId
    case ret of
      Left err ->
        putStrLn $ "Handshake failed: " ++ err
      Right (sock, infoHash', peerId', extra) -> do
        putStrLn "Handshake successful"
        peers' <- takeMVar peers
        case M.lookup addr peers' of
          Nothing -> do
            -- first time handshaking with the peer, spawn a socket
            -- listener
            void $ async $ listenConnectedSock sock addr peers
            -- TODO: check info_hash
            let peerConn = newPeerConn peerId' infoHash' sock
            putMVar peers $ M.insert addr peerConn peers'
          Just pc -> do
            -- TODO: I don't know how can this happen. We already
            -- established a connection. Just reset the peer info.
            putMVar peers $ M.insert addr (newPeerConn peerId' infoHash' sock) peers'
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
    if B.null bytes
      then return $ Left $ "Handshake refused"
      else case parseHandshake bytes of
             Left err -> do
               warning $ err ++ " msg: " ++ show (B.unpack bytes)
               return $ Left err
             Right (infoHash', peerId', extra) -> return $ Right (sock, infoHash', peerId', extra)
  where
    errHandler err@IOError{ioe_type=NoSuchThing} =
      return $ Left $ "Problems with connection: " ++ show err
    errHandler err@IOError{ioe_type=TimeExpired} =
      return $ Left $ "Timeout happened: " ++ show err
    errHandler err =
      return $ Left $ "Unhandled error: " ++ show err

-- * Logging stuff

-- | Logger used in this module.
logger :: String
logger = "Rho.PeerComms"

warning :: String -> IO ()
warning = L.warningM logger

errorLog :: String -> IO ()
errorLog = L.errorM logger
