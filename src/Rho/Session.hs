{-# LANGUAGE NondecreasingIndentation, OverloadedStrings #-}

-- | Handling all the state required to download a single torrent.
-- A new session is created for every torrent.
module Rho.Session where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad
import qualified Data.BEncode                 as BE
import qualified Data.ByteString              as B
import qualified Data.ByteString.Lazy         as LB
import           Data.IORef
import qualified Data.Map                     as M
import           Data.Word
import           GHC.IO.Exception
import           Network.Socket               hiding (KeepAlive, recv, recvFrom,
                                               recvLen, send, sendTo)
import           Network.Socket.ByteString
import           System.IO.Error
import qualified System.Log.Logger            as L

import           Rho.InfoHash
import           Rho.Listener                 (Listener, initListener, recvLen,
                                               stopListener)
import           Rho.Magnet
import           Rho.Metainfo
import           Rho.PeerComms.Handshake
import           Rho.PeerComms.Message
import           Rho.PeerComms.PeerConnection hiding (errorLog, logger, warning)
import           Rho.PeerComms.PeerConnState
import           Rho.PieceMgr
import           Rho.SessionState

-- | Initialize listeners, data structures etc. for peer communications,
-- using magnet URI.
initMagnetSession :: PortNumber -> Magnet -> PeerId -> IO () -> IO () -> IO Session
initMagnetSession port m pid mic tc = initMagnetSession' port 0 m pid mic tc

-- | Initialize listeners, data structures etc. for peer communications,
-- using info dictionary.
initTorrentSession :: PortNumber -> Info -> PeerId -> IO () -> IO Session
initTorrentSession port info pid tc = initTorrentSession' port 0 info pid tc

initMagnetSession' :: PortNumber -> HostAddress -> Magnet -> PeerId -> IO () -> IO () -> IO Session
initMagnetSession' port host (Magnet ih _ _) pid mic tc = do
    peers      <- newMVar M.empty
    pieceMgr   <- newMVar Nothing
    miPieceMgr <- newMVar Nothing
    onMIComplete <- newMVar mic
    onTorrentComplete <- newMVar tc
    sock <- socket AF_INET Stream defaultProtocol
    bind sock (SockAddrInet port host)
    listen sock 1
    let sess    = Session pid ih peers pieceMgr miPieceMgr onMIComplete onTorrentComplete
    void $ async $ listenPeerSocket sess sock
    return sess

initTorrentSession' :: PortNumber -> HostAddress -> Info -> PeerId -> IO () -> IO Session
initTorrentSession' port host info pid tc = do
    peers      <- newMVar M.empty
    pieceMgr   <- newMVar . Just =<< newPieceMgr (torrentSize info) (iPieceLength info)
    let miData  = LB.toStrict $ BE.encode info
    miPieceMgr <- newMVar . Just =<< newPieceMgrFromData miData (2 ^ (14 :: Word32))
    onMIComplete <- newMVar (return ())
    onTorrentComplete <- newMVar tc
    sock       <- socket AF_INET Stream defaultProtocol
    bind sock (SockAddrInet port host)
    listen sock 1
    let sess    = Session pid (iHash info) peers pieceMgr miPieceMgr onMIComplete onTorrentComplete
    void $ async $ listenPeerSocket sess sock
    return sess

-- | Accept incoming connections and spawn a connected socket listener for
-- every accepted connection.
listenPeerSocket :: Session -> Socket -> IO ()
listenPeerSocket sess sock = do
    (peerSock, peerAddr) <- accept sock
    listener <- initListener $ recv peerSock 4096
    -- we should have a handshake message as first thing
    void $ async $ listenHandshake sess listener peerSock peerAddr
    listenPeerSocket sess sock

-- | Wait for an incoming handshake, update peers state upon successfully
-- parsing the handshake and continue listening the connected socket.
listenHandshake :: Session -> Listener -> Socket -> SockAddr -> IO ()
listenHandshake sess listener sock peerAddr = do
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
            sendAll sock $ mkHandshake (hInfoHash hs) (sessPeerId sess)
            handleHandshake sess sock peerAddr listener hs

handshake :: Session -> SockAddr -> InfoHash -> IO (Either String ExtendedMsgSupport)
handshake sess@Session{sessPeerId=peerId} addr infoHash = do
    ret <- sendHandshake addr infoHash peerId
    case ret of
      Left err -> do
        putStrLn $ "Handshake failed: " ++ err
        return $ Left err
      Right (sock, listener, hs) -> do
        putStrLn $ "Handshake successful. Extension support: " ++ show (hExtension hs)
        handleHandshake sess sock addr listener hs
        return $ Right (hExtension hs)

-- | Send a handshake message to given target using a fresh socket. Return
-- the connected socket in case of a success. (e.g. receiving answer to
-- handshake)
sendHandshake :: SockAddr -> InfoHash -> PeerId -> IO (Either String (Socket, Listener, Handshake))
sendHandshake addr infoHash peerId = flip catchIOError errHandler $ do
    sock <- socket AF_INET Stream defaultProtocol
    bind sock (SockAddrInet aNY_PORT 0)
    putStrLn $ "Sending handshake to remote: " ++ show addr
    let msg = mkHandshake infoHash peerId
    connect sock addr
    listener <- initListener $ recv sock 4096
    sendAll sock msg
    incomingHs <- recvHandshake listener
    case incomingHs of
      ConnClosed hs
        | B.null hs ->
            stopListener listener >> return (Left "refused")
        | otherwise ->
            stopListener listener >> return (Left $ "partial message: " ++ show (B.unpack hs))
      Msg hs -> case parseHandshake hs of
                  Left err -> do
                    stopListener listener
                    warning $ err ++ " msg: " ++ show (B.unpack hs)
                    return $ Left err
                  Right hs' -> return $ Right (sock, listener, hs')
  where
    errHandler err@IOError{ioe_type=NoSuchThing} =
      return $ Left $ "Problems with connection: " ++ show err
    errHandler err@IOError{ioe_type=TimeExpired} =
      return $ Left $ "Timeout happened: " ++ show err
    errHandler err =
      return $ Left $ "Unhandled error: " ++ show err

sendExtendedHs :: Session -> PeerConn -> IO ()
sendExtendedHs sess pc = do
    mi <- readMVar $ sessMIPieceMgr sess
    case mi of
      Nothing -> do
        -- we don't know metainfo size
        void $ sendMessage pc (Extended $ defaultExtendedHs Nothing)
      Just pm ->
        void $ sendMessage pc (Extended $ defaultExtendedHs $ Just $ pmTotalSize pm)

-- | Process incoming handshake; update data structures, spawn socket
-- listener.
handleHandshake :: Session -> Socket -> SockAddr -> Listener -> Handshake -> IO ()
handleHandshake sess@Session{sessInfoHash=ih, sessPeers=peers} sock addr listener hs
  | ih == hInfoHash hs = do
      peers' <- takeMVar peers
      case M.lookup addr peers' of
        Nothing -> do
          let pc    = newPeerConn (hPeerId hs) (hInfoHash hs) (hExtension hs) sock addr
          peerConn <- newIORef pc
          void $ async $ do
            listenConnectedSock sess peerConn listener
            modifyMVar_ peers $ return . M.delete addr
          sendExtendedHs sess pc
          putMVar peers $ M.insert addr peerConn peers'
        Just _ -> do
          -- TODO: I don't know how can this happen. We already established
          -- a connection. Just reset the peer info.
          peerConn <- newIORef $ newPeerConn (hPeerId hs) (hInfoHash hs) (hExtension hs) sock addr
          putMVar peers $ M.insert addr peerConn peers'
    | otherwise = do
        warning $ "Got handshake for a different torrent: " ++ show (hInfoHash hs)

-- | Try to recv a message of length 68.
recvHandshake :: Listener -> IO RecvMsg
recvHandshake listener = do
    msg <- recvLen listener 68
    return $ (if B.length msg /= 68 then ConnClosed else Msg) msg

-- * Logging stuff

-- | Logger used in this module.
logger :: String
logger = "Rho.Session"

warning :: String -> IO ()
warning = L.warningM logger

errorLog :: String -> IO ()
errorLog = L.errorM logger
