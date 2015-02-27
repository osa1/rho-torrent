{-# LANGUAGE NondecreasingIndentation, OverloadedStrings #-}

-- | Handling all the state required to download a single torrent.
-- A new session is created for every torrent.
module Rho.Session where

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad
import qualified Data.BEncode                  as BE
import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as LB
import           Data.IORef
import qualified Data.Map                      as M
import           Data.Monoid
import qualified Data.Set                      as S
import           Data.Word
import           Network.Socket                hiding (KeepAlive, recv,
                                                recvFrom, recvLen, send, sendTo)
import           Network.Socket.ByteString
import           System.Directory              (createDirectoryIfMissing)
import           System.FilePath               (takeDirectory)
import qualified System.Log.Logger             as L

import           Rho.InfoHash
import           Rho.Listener                  (Listener, initListener,
                                                stopListener)
import           Rho.ListenerUtils
import           Rho.Magnet
import           Rho.Metainfo
import           Rho.PeerComms.Handshake
import           Rho.PeerComms.Message
import           Rho.PeerComms.PeerConnection  hiding (errorLog, info, logger,
                                                notice, warning)
import           Rho.PeerComms.PeerConnState
import           Rho.PeerComms.PeerId
import           Rho.PieceMgr                  hiding (notice)
import           Rho.SessionState
import           Rho.Tracker
import           Rho.TrackerComms.PeerRequest
import           Rho.TrackerComms.PeerResponse

-- | Initialize listeners, data structures etc. for peer communications,
-- using magnet URI.
initMagnetSession :: Magnet -> PeerId -> IO Session
initMagnetSession = initMagnetSession' 0

-- | Initialize listeners, data structures etc. for peer communications,
-- using info dictionary.
initTorrentSession :: Info -> [Tracker] -> PeerId -> IO Session
initTorrentSession = initTorrentSession' 0

initMagnetSession' :: HostAddress -> Magnet -> PeerId -> IO Session
initMagnetSession' host (Magnet ih ts _) pid = do
    sock <- socket AF_INET Stream defaultProtocol
    bind sock (SockAddrInet aNY_PORT host)
    port <- socketPort sock
    listen sock 1
    sess <- initSession pid ih port ts Nothing Nothing
    void $ async $ listenPeerSocket sess sock
    return sess

initTorrentSession' :: HostAddress -> Info -> [Tracker] -> PeerId -> IO Session
initTorrentSession' host info ts pid = do
    (pieceMgr, _) <- tryReadFiles info ""
    let miData  = LB.toStrict $ BE.encode info
    miPieceMgr <- Just <$> newPieceMgrFromData miData (2 ^ (14 :: Word32))
    sock       <- socket AF_INET Stream defaultProtocol
    bind sock (SockAddrInet aNY_PORT host)
    port       <- socketPort sock
    listen sock 1
    sess <- initSession pid (iHash info) port ts (Just pieceMgr) miPieceMgr
    void $ async $ listenPeerSocket sess sock
    return sess

runMagnetSession :: Session -> IO Bool
runMagnetSession sess@Session{sessInfoHash=hash, sessTrackers=ts} = do
    ts' <- readMVar ts
    PeerResponse _ _ _ peers <- mconcat <$> mapM (requestPeers sess) ts'
    forM_ peers $ \peer -> void $ forkIO $ void $ handshake sess peer hash
    notice $ "Waiting 5 seconds to establish connections with "
               ++ show (length peers) ++ " peers."
    threadDelay (1000000 * 5)
    notice "Blocking until learning metainfo size from peers..."
    miPieceMgr <- readMVar (sessMIPieceMgr sess)
    miDone <- newEmptyMVar
    modifyMVar_ (sessOnMIComplete sess) (\_ -> return $ putMVar miDone ())

    loopThread   <- async $ loop miPieceMgr
    miDoneThread <- async $ void $ readMVar miDone

    -- loop thread never terminates, I'm just using `waitAnyCancel` to
    -- interrupt loop thread when metainfo download is complete.
    void $ waitAnyCancel [loopThread, miDoneThread]

    notice "Downloaded the info. Parsing..."
    bytes <- getBytes miPieceMgr
    case parseInfoDict bytes of
      Left err   -> error $ "Can't parse info dict: " ++ err
      Right info
        | iHash info == hash -> do
            notice "Hash correct"
            runTorrentSession sess info
        | otherwise -> do
            notice "Wrong hash"
            return False
  where
    loop pieces = do
      peersMap <- readMVar $ sessPeers sess
      sendMetainfoRequests peersMap pieces
      threadDelay (1000000 * 5)
      loop pieces

runTorrentSession :: Session -> Info -> IO Bool
runTorrentSession sess@Session{sessPeers=peers, sessPieceMgr=pieces, sessRequestedPieces=requests,
                               sessInfoHash=hash, sessTrackers=ts} info = do
    ts' <- readMVar ts
    PeerResponse _ _ _ peers' <- mconcat <$> mapM (requestPeers sess) ts'
    connectedPeers <- M.keysSet <$> readMVar peers
    let newPeers = S.fromList peers' `S.difference` connectedPeers
    forM_ (S.toList newPeers) $ \peer -> void $ forkIO $ void $ handshake sess peer hash

    -- initialize piece manager
    pieces' <- takeMVar pieces
    pmgr <- case pieces' of
              Nothing   -> do
                pmgr <- fst <$> tryReadFiles info ""
                putMVar pieces (Just pmgr)
                return pmgr
              Just pmgr -> do
                putMVar pieces pieces'
                return pmgr

    -- set the callback
    torrentDone <- newEmptyMVar
    modifyMVar_ (sessOnTorrentComplete sess) $ \cb -> return (putMVar torrentDone () >> cb)
    torrentDoneThread <- async $ readMVar torrentDone

    -- start the loop
    loopThread <- async $ loop pmgr

    -- loop until the torrent is complete
    void $ waitAnyCancel [loopThread, torrentDoneThread]

    notice "Torrent is complete. Checking hashes of pieces."
    checks <- zipWithM (checkPieceHash pmgr) [0..] (iPieces info)
    if not (and checks)
      then do
        notice "Some of the hashes don't match."
        return False
      else do
        notice "Torrent successfully downloaded. Generating files."
        writeFiles =<< generateFiles pmgr info
        return True
  where
    loop pmgr = do
      peersMap <- readMVar peers
      reqs <- readMVar requests
      sendPieceRequests peersMap reqs pmgr
      threadDelay (1000000 * 5)
      loop pmgr

    writeFiles :: [(FilePath, B.ByteString)] -> IO ()
    writeFiles [] = return ()
    writeFiles ((f, c) : rest) = do
      notice $ "Writing file: " ++ f
      createDirectoryIfMissing True (takeDirectory f)
      B.writeFile f c
      writeFiles rest

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
        | B.null bs -> notice "Got weird handshake attempt."
        | otherwise -> notice $ "Got partial handshake msg: " ++ show bs
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
        notice $ "Handshake failed: " ++ err
        return $ Left err
      Right (sock, listener, hs) -> do
        notice $ "Handshake successful. Extension support: " ++ show (hExtension hs)
        handleHandshake sess sock addr listener hs
        return $ Right (hExtension hs)

-- | Send a handshake message to given target using a fresh socket. Return
-- the connected socket in case of a success. (e.g. receiving answer to
-- handshake)
sendHandshake :: SockAddr -> InfoHash -> PeerId -> IO (Either String (Socket, Listener, Handshake))
sendHandshake addr infoHash peerId = do
    sock <- socket AF_INET Stream defaultProtocol
    bind sock (SockAddrInet aNY_PORT 0)
    notice $ "Sending handshake to remote: " ++ show addr
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

sendExtendedHs :: Session -> PeerConn -> IO ()
sendExtendedHs sess pc = do
    mi <- tryReadMVar $ sessMIPieceMgr sess
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
          let pc    = newPeerConn (hPeerId hs) (hInfoHash hs) (hExtension hs) sock addr listener
          peerConn <- newIORef pc
          void $ async $ do
            listenConnectedSock sess peerConn listener
            modifyMVar_ peers $ return . M.delete addr
          sendBitfield sess pc
          sendExtendedHs sess pc
          putMVar peers $ M.insert addr peerConn peers'
        Just _ -> do
          -- TODO: I don't know how can this happen.
          warning $ "Got a handshake from a peer we've already connected: " ++ show addr
    | otherwise = do
        warning $ "Got handshake for a different torrent: " ++ show (hInfoHash hs)

sendBitfield :: Session -> PeerConn -> IO ()
sendBitfield Session{sessPieceMgr=pieces} pc = do
    pmgr <- readMVar pieces
    case pmgr of
      Nothing    -> return () -- we don't have any pieces
      Just pmgr' -> void . sendMessage pc . Bitfield =<< makeByteString pmgr'

-- * Logging stuff

-- | Logger used in this module.
logger :: String
logger = "Rho.Session"

warning, notice :: String -> IO ()
warning  = L.warningM logger
notice   = L.noticeM logger
