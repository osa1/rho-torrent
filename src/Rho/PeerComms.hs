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
  } deriving (Show)

data PeerCommHandler = PeerCommHandler
  { pchPeers   :: MVar (M.Map SockAddr PeerConn)
  , pchMsgChan :: Chan (SockAddr, Socket, B.ByteString)
  -- , pchSock  :: Socket
    -- ^ socket used to listen incoming messages
  }

-- | Initialize listeners, data structures etc. for peer communications.
initPeerCommsHandler :: IO PeerCommHandler
initPeerCommsHandler = do
    -- state of all connected peers
    peers <- newMVar M.empty
    -- sock for incoming connections
    sock <- socket AF_INET Stream defaultProtocol
    bind sock (SockAddrInet (fromIntegral (5433 :: Int)) 0) -- TODO: hard-coded port
    -- channel from from socket listeners to message handler
    msgChan <- newChan
    _ <- spawnPeerSockListener sock msgChan
    async $ peerMsgHandler peers msgChan
    return $ PeerCommHandler peers msgChan

-- | Accept incoming connections and spawn a connected socket listener for
-- every accepted connection.
spawnPeerSockListener :: Socket -> Chan (SockAddr, Socket, B.ByteString) -> IO (Async ())
spawnPeerSockListener sock msgChan = async loop
  where
    loop = do
      (peerSocket, peerAddr) <- accept sock
      spawnConnectedSockListener peerSocket peerAddr msgChan
      loop

-- | Listen socket of a connected peer and send bytes to channel.
spawnConnectedSockListener
    :: Socket -> SockAddr -> Chan (SockAddr, Socket, B.ByteString) -> IO (Async ())
spawnConnectedSockListener sock sockAddr msgChan = async loop
  where
    loop = do
      msg <- recv sock 10000
      -- empty message == closed connection
      if B.null msg
        then putStrLn "Closing connection with a peer."
        else do
          writeChan msgChan (sockAddr, sock, msg)
          loop

-- | Parse incoming bytes and update data structures.
peerMsgHandler :: MVar (M.Map SockAddr PeerConn) -> Chan (SockAddr, Socket, B.ByteString) -> IO ()
peerMsgHandler peers msgChan = do
    (peerAddr, peerSock, peerMsg) <- readChan msgChan
    handlePeerMsg peers peerAddr peerSock peerMsg
    peerMsgHandler peers msgChan

handlePeerMsg :: MVar (M.Map SockAddr PeerConn) -> SockAddr -> Socket -> B.ByteString -> IO ()
handlePeerMsg peers peerAddr peerSock peerMsg = do
    -- have we established a connection with the peer?
    peers' <- readMVar peers
    case M.lookup peerAddr peers' of
      Nothing ->
        -- message has to be handshake
        case parseHandshake peerMsg of
          Left err -> putStrLn $ "Can't parse handshake: " ++ err
          Right (_infoHash, _peerId, _msg) -> do
            -- TODO: we don't seed yet
            putStrLn "Ignoring an incoming handshake."
      Just peerConn ->
        -- TODO: handle peer msg tables
        case parsePeerMsg peerMsg of
          Left err  -> putStrLn $ "Can't parse peer msg: " ++ err
          Right msg -> putStrLn $ "Parsed a peer msg: " ++ show msg

-- | Send extended handshake to request metainfo.
requestMetainfo :: PeerConn -> IO ()
requestMetainfo PeerConn{pcSock=sock} = do
    let msg = "d8:msg_typei0e5:piecei0ee"
    sent <- send sock msg
    unless (sent == B.length msg) $ putStrLn "Problem while sending extended handshake"

handshake :: PeerCommHandler -> SockAddr -> InfoHash -> PeerId -> IO ()
handshake PeerCommHandler{pchPeers=peers, pchMsgChan=msgChan} addr infoHash peerId = do
    ret <- sendHandshake addr infoHash peerId
    case ret of
      Left err -> putStrLn $ "Can't establish connection: " ++ err
      Right (sock, infoHash, peerId, msg) -> do
        modifyMVar_ peers $ \peers' ->
          case M.lookup addr peers' of
            Nothing -> do
              -- first time handshaking with the peer, spawn a socket
              -- listener
              spawnConnectedSockListener sock addr msgChan
              -- TODO: check info_hash
              putStrLn "Handshake successful"
              return $ M.insert addr (PeerConn True False True False peerId [infoHash] sock) peers'
            Just pc -> do
              -- probably learned about a new torrent
              putStrLn "Handshake successful"
              -- we already knew about this peer, so there should be
              -- a thread listening for messages from this socket. no need
              -- to create a new one.
              return $ M.insert addr pc{pcOffers = infoHash : pcOffers pc} peers'
        writeChan msgChan (addr, sock, msg)

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
    msg <- recv sock 10000
    case parseHandshake msg of
      Left err -> return $ Left err
      Right (infoHash, peerId, msg) -> return $ Right (sock, infoHash, peerId, msg)
  where
    errHandler err@IOError{ioe_type=NoSuchThing} =
      return $ Left $ "Problems with connection: " ++ show err
    errHandler err@IOError{ioe_type=TimeExpired} =
      return $ Left $ "Timeout happened: " ++ show err
    errHandler err =
      return $ Left $ "Unhandled error: " ++ show err
