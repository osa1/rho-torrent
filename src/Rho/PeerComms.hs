{-# LANGUAGE NondecreasingIndentation, OverloadedStrings #-}

-- | Communications with peers.
module Rho.PeerComms where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad
import qualified Data.ByteString           as B
import qualified Data.Map                  as M
import           Data.Maybe
import           Data.Monoid
import           Data.Word
import           GHC.IO.Exception
import           Network.Socket            hiding (KeepAlive, recv, recvFrom,
                                            recvLen, send, sendTo)
import           Network.Socket.ByteString
import           System.IO.Error
import qualified System.Log.Logger         as L

import qualified Rho.Bitfield              as BF
import           Rho.InfoHash
import           Rho.Listener              (Listener, initListener, recvLen)
import           Rho.Metainfo
import           Rho.PeerComms.Handshake
import           Rho.PeerComms.Message
import           Rho.PieceMgr
import           Rho.Utils

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
  , pcExtended       :: ExtendedMsgSupport
    -- ^ Supports BEP10
  , pcExtendedMsgTbl :: ExtendedPeerMsgTable
    -- ^ BEP10, extension table
  , pcMetadataSize   :: Maybe Word32
    -- ^ BEP9, metadata_size key of ut_metadata handshake
  } deriving (Show)

newPeerConn :: PeerId -> InfoHash -> ExtendedMsgSupport -> Socket -> PeerConn
newPeerConn peerId infoHash extension sock =
    PeerConn True False True False peerId infoHash Nothing sock extension M.empty Nothing

-- | Stat of peers, shared between workers.
type PeersState = MVar (M.Map SockAddr PeerConn)

data PeerCommHandler = PeerCommHandler
  { pchPeers    :: PeersState
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
    -- have we established a connection with the peer?
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

-- | Listen a connected socket and handle incoming messages.
listenConnectedSock :: PeerCommHandler -> Listener -> Socket -> SockAddr -> IO ()
listenConnectedSock comms@(PeerCommHandler peers _ _ ) listener sock sockAddr =
    flip catchIOError errHandler $ loop
  where
    loop = do
      msg <- recvMessage listener
      case msg of
        ConnClosed msg
          | B.null msg -> closeConn
          | otherwise  -> putStrLn ("recvd a partial message: " ++ show (B.unpack msg)) >> closeConn
        Msg msg -> handleMessage comms msg sock sockAddr >> loop

    errHandler err = do
      putStrLn $ "Error happened while listening a socket: " ++ show err
                  ++ ". Closing the connection."
      closeConn

    closeConn = modifyMVar_ peers $ return . M.delete sockAddr

handleMessage :: PeerCommHandler -> B.ByteString -> Socket -> SockAddr -> IO ()
handleMessage (PeerCommHandler peers pieces _) msg _sock peerAddr = do
    case parsePeerMsg msg of
      Left err -> warning . concat $
        [ "Can't parse peer message: ", err,
          " msg: ", show msg, " msg length: ", show (B.length msg) ]
      Right KeepAlive -> return () -- TODO: should I ignore keep-alives?
      Right (Bitfield bf) -> modifyPeerState $ \pc -> pc{pcPieces = Just bf}
      Right (Have piece) ->
        modifyPeerState $ \pc ->
          let bf' = Just $ BF.set (fromMaybe BF.empty $ pcPieces pc) (fromIntegral piece)
          in pc{pcPieces = bf'}
      Right Choke -> modifyPeerState $ \pc -> pc{pcChoking = True}
      Right Unchoke -> modifyPeerState $ \pc -> pc{pcChoking = False}
      Right Interested -> modifyPeerState $ \pc -> pc{pcPeerInterested = True}
      Right NotInterested -> modifyPeerState $ \pc -> pc{pcPeerInterested = False}
      Right (Piece pIdx offset pData) -> do
        putStrLn "Got piece response"
        writePiece pieces pIdx offset pData
      Right (Extended (ExtendedHandshake tbl [UtMetadataSize size])) -> do
        putStrLn "Got extended handshake."
        withPeer $ \peers' pc -> do
          -- TODO: We should send extended handshake before receiving one
          sendMessage pc (Extended (ExtendedHandshake defaultMsgTable []))
          return $ M.insert peerAddr pc{pcExtendedMsgTbl = tbl, pcMetadataSize = Just size} peers'
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
        void $ async $ listenConnectedSock comms listener sock addr
        -- TODO: check info_hash
        let peerConn = newPeerConn (hPeerId hs) (hInfoHash hs) (hExtension hs) sock
        putMVar peers $ M.insert addr peerConn peers'
      Just pc -> do
        -- TODO: I don't know how can this happen. We already established
        -- a connection. Just reset the peer info.
        putMVar peers $
          M.insert addr (newPeerConn (hPeerId hs) (hInfoHash hs) (hExtension hs) sock) peers'

sendMessage :: PeerConn -> PeerMsg -> IO (Maybe String)
sendMessage PeerConn{pcSock=sock, pcExtendedMsgTbl=tbl} msg =
    case mkPeerMsg tbl msg of
      Left err -> return $ Just err
      Right bytes -> send sock bytes >> return Nothing

sendPieceRequests :: PeerCommHandler -> IO [PeerConn]
sendPieceRequests (PeerCommHandler peers pieces@(PieceMgr ps _ _ _) _) = do
    -- TODO: fix horrible piece request algortihm
    missings <- missingPieces pieces
    putStrLn $ "Missing pieces: " ++ show missings
    peerComms <- M.elems `fmap` readMVar peers
    sent <- forM missings $ \(pIdx, pOffset, pSize) -> do
      putStrLn $ "all peers: " ++ show (map (unwrapPeerId . pcPeerId) peerComms)
      let peersWithPiece = flip filter peerComms $ \PeerConn{pcPieces=pieces} ->
                                                      case pieces of
                                                        Nothing -> False
                                                        Just ps -> BF.test ps (fromIntegral pIdx)
          unchokedPeers = filter (not . pcChoking) peersWithPiece
      putStrLn $ "unchoked peers: " ++ show (map (unwrapPeerId . pcPeerId) unchokedPeers)
      case unchokedPeers of
        [] -> return Nothing
        (p : _) -> do
          putStrLn $ "Sending peer message to peer: " ++ show (unwrapPeerId $ pcPeerId p)
          ret <- sendMessage p $ Request pIdx pOffset pSize
          case ret of
            Nothing -> return $ Just p
            Just err -> do
              putStrLn $ "Can't send piece request: " ++ err
              return Nothing

    return $ catMaybes sent

-- * Receive helpers

data RecvMsg = ConnClosed B.ByteString | Msg B.ByteString deriving (Show, Eq)

-- | Try to receive a 4-byte length-prefixed message.
recvMessage :: Listener -> IO RecvMsg
recvMessage listener = do
    lengthPrefix <- recvLen listener 4
    if B.length lengthPrefix /= 4
      then return $ ConnClosed lengthPrefix
      else do
    let [w1, w2, w3, w4] = B.unpack lengthPrefix
        len = mkWord32 w1 w2 w3 w4
    msg <- recvLen listener (fromIntegral len)
    return $ Msg $ lengthPrefix <> msg

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
