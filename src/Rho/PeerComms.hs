{-# LANGUAGE NondecreasingIndentation, OverloadedStrings #-}

-- | Communications with peers.
module Rho.PeerComms where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad
import qualified Data.ByteString              as B
import           Data.IORef
import qualified Data.Map                     as M
import           GHC.IO.Exception
import           Network.Socket               hiding (KeepAlive, recv, recvFrom,
                                               recvLen, send, sendTo)
import           Network.Socket.ByteString
import           System.IO.Error
import qualified System.Log.Logger            as L

import           Rho.InfoHash
import           Rho.Listener                 (Listener, initListener, recvLen)
import           Rho.Metainfo
import           Rho.PeerComms.Handshake
import           Rho.PeerComms.PeerConnection hiding (errorLog, logger, warning)
import           Rho.PieceMgr

data PeerCommHandler = PeerCommHandler
  { pchPeers    :: MVar (M.Map SockAddr (IORef PeerConn))
  , pchPieceMgr :: PieceMgr
  , pchPeerId   :: PeerId
    -- ^ our peer id
  }

-- | Initialize listeners, data structures etc. for peer communications.
initPeerCommsHandler :: Info -> PeerId -> IO PeerCommHandler
initPeerCommsHandler torrentInfo pid = do
    pieceMgr <- newPieceMgr (fromIntegral $ torrentSize torrentInfo)
                            (fromIntegral $ iPieceLength torrentInfo)
    -- state of all connected peers
    peers <- newMVar M.empty
    -- sock for incoming connections
    sock <- socket AF_INET Stream defaultProtocol
    bind sock (SockAddrInet (fromIntegral (5433 :: Int)) 0) -- TODO: hard-coded port
    let ret = PeerCommHandler peers pieceMgr pid
    -- We don't block the thread that listens for incoming handshakes
    void $ async $ listenPeerSocket ret sock
    return ret

-- | Accept incoming connections and spawn a connected socket listener for
-- every accepted connection.
listenPeerSocket :: PeerCommHandler -> Socket -> IO ()
listenPeerSocket comms sock = do
    (peerSock, peerAddr) <- accept sock
    listener <- initListener $ recv peerSock 4096
    -- we should have a handshake message as first thing
    void $ async $ listenHandshake comms listener peerSock peerAddr
    listenPeerSocket comms sock

-- | Wait for an incoming handshake, update peers state upon successfully
-- parsing the handshake and continue listening the connected socket.
listenHandshake :: PeerCommHandler -> Listener -> Socket -> SockAddr -> IO ()
listenHandshake comms listener sock peerAddr = do
    msg <- recvHandshake listener
    case msg of
      ConnClosed bs
        | B.null bs -> putStrLn "Got weird handshake attempt."
        | otherwise -> putStrLn $ "Got partial handshake msg: " ++ show bs
      Msg bs ->
        case parseHandshake bs of
          Left err ->
            warning $ "Can't parse handshake: " ++ err ++ " msg: " ++ show bs
          Right hs -> do
            -- TODO: we probably need to check info_hash
            send sock $ mkHandshake (hInfoHash hs) (pchPeerId comms)
            handleHandshake comms sock peerAddr hs

handshake :: PeerCommHandler -> SockAddr -> InfoHash -> IO ()
handshake comms@(PeerCommHandler peers pieces peerId) addr infoHash = do
    ret <- sendHandshake addr infoHash peerId
    case ret of
      Left err ->
        putStrLn $ "Handshake failed: " ++ err
      Right (sock, hs) -> do
        putStrLn $ "Handshake successful. Extension support: " ++ show (hExtension hs)
        handleHandshake comms sock addr hs

-- | Send a handshake message to given target using a fresh socket. Return
-- the connected socket in case of a success. (e.g. receiving answer to
-- handshake)
sendHandshake :: SockAddr -> InfoHash -> PeerId -> IO (Either String (Socket, Handshake))
sendHandshake addr infoHash peerId = flip catchIOError errHandler $ do
    sock <- socket AF_INET Stream defaultProtocol
    bind sock (SockAddrInet aNY_PORT 0)
    putStrLn $ "Sending handshake to remote: " ++ show addr
    let msg = mkHandshake infoHash peerId
    connect sock addr
    listener <- initListener $ recv sock 4096
    sent <- send sock msg
    incomingHs <- recvHandshake listener
    case incomingHs of
      ConnClosed hs
        | B.null hs -> return $ Left "refused"
        | otherwise  -> return $ Left $ "partial message: " ++ show (B.unpack hs)
      Msg hs -> case parseHandshake hs of
                  Left err -> do
                    warning $ err ++ " msg: " ++ show (B.unpack hs)
                    return $ Left err
                  Right hs' -> return $ Right (sock, hs')
  where
    errHandler err@IOError{ioe_type=NoSuchThing} =
      return $ Left $ "Problems with connection: " ++ show err
    errHandler err@IOError{ioe_type=TimeExpired} =
      return $ Left $ "Timeout happened: " ++ show err
    errHandler err =
      return $ Left $ "Unhandled error: " ++ show err

-- | Process incoming handshake; update data structures, spawn socket
-- listener.
handleHandshake :: PeerCommHandler -> Socket -> SockAddr -> Handshake -> IO ()
handleHandshake comms@(PeerCommHandler peers pieces _) sock addr hs = do
    peers' <- takeMVar peers
    case M.lookup addr peers' of
      Nothing -> do
        -- first time handshaking with the peer, spawn a socket listener
        listener <- initListener $ recv sock 4096
        peerConn <- newPeerConn (hPeerId hs) (hInfoHash hs) (hExtension hs) sock
        void $ async $ do
          listenConnectedSock peerConn pieces listener sock addr
          modifyMVar_ peers $ return . M.delete addr
        -- TODO: check info_hash
        putMVar peers $ M.insert addr peerConn peers'
      Just pc -> do
        -- TODO: I don't know how can this happen. We already established
        -- a connection. Just reset the peer info.
        peerConn <- newPeerConn (hPeerId hs) (hInfoHash hs) (hExtension hs) sock
        putMVar peers $ M.insert addr peerConn peers'

-- | Try to recv a message of length 68.
recvHandshake :: Listener -> IO RecvMsg
recvHandshake listener = do
    msg <- recvLen listener 68
    return $ (if B.length msg /= 68 then ConnClosed else Msg) msg

-- * Logging stuff

-- | Logger used in this module.
logger :: String
logger = "Rho.PeerComms"

warning :: String -> IO ()
warning = L.warningM logger

errorLog :: String -> IO ()
errorLog = L.errorM logger
