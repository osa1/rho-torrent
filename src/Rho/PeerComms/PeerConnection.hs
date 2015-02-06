{-# LANGUAGE LambdaCase, NondecreasingIndentation, OverloadedStrings,
             TupleSections #-}

-- | Handling communications with peers after a successful handshake.
module Rho.PeerComms.PeerConnection where

import           Control.Applicative
import           Control.Concurrent
import           Control.Monad
import qualified Data.ByteString             as B
import           Data.IORef
import           Data.List                   (find)
import qualified Data.Map                    as M
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set                    as S
import           Data.Word
import           Network.Socket              hiding (KeepAlive, recv, recvFrom,
                                              recvLen, send, sendTo)
import           Network.Socket.ByteString
import           System.IO.Error
import qualified System.Log.Logger           as L

import qualified Rho.Bitfield                as BF
import           Rho.Listener                (Listener, recvLen)
import           Rho.PeerComms.Message
import           Rho.PeerComms.PeerConnState
import           Rho.PeerComms.PeerPieceAsgn
import           Rho.PieceMgr
import           Rho.SessionState
import           Rho.Utils

-- | Listen a connected socket and handle incoming messages.
listenConnectedSock :: Session -> IORef PeerConn -> Listener -> IO ()
listenConnectedSock sess peer listener = flip catchIOError errHandler loop
  where
    loop = do
      msg <- recvMessage listener
      case msg of
        ConnClosed msg'
          | B.null msg' -> return ()
          | otherwise  -> putStrLn ("recvd a partial message: " ++ show (B.unpack msg'))
        Msg msg' -> handleMessage sess peer msg' >> loop

    errHandler err = do
      putStrLn $ "Error happened while listening a socket: " ++ show err
                  ++ ". Closing the connection."

handleMessage :: Session -> IORef PeerConn -> B.ByteString -> IO ()
handleMessage sess peer msg = do
    case parsePeerMsg msg of
      Left err -> warning . concat $
        [ "Can't parse peer message: ", err,
          " msg: ", show msg, " msg length: ", show (B.length msg) ]
      Right msg' -> handleMessage' sess peer msg'

handleMessage' :: Session -> IORef PeerConn -> PeerMsg -> IO ()
handleMessage' _ _ KeepAlive = return () -- TODO: should we ignore keep-alives?

handleMessage' sess peer (Bitfield bytes) = do
    pm <- takeMVar $ sessPieceMgr sess
    case pm of
      Nothing -> do
        -- we don't know how many pieces we have yet, just set it using
        -- parsed bitfield
        bf <- BF.fromBS bytes (B.length bytes * 8)
        atomicModifyIORef_ peer $ \pc -> pc{pcPieces = Just bf}
      Just pm' -> do
        -- TODO: Check spare bits and close the connection if they're
        -- not 0
        bf <- BF.fromBS bytes (fromIntegral $ pmPieces pm')
        atomicModifyIORef_ peer $ \pc -> pc{pcPieces = Just bf}
    putMVar (sessPieceMgr sess) pm

handleMessage' _ peer (Have piece) = do
    pc <- readIORef peer
    case pcPieces pc of
      Nothing -> do
        -- we need to initialize bitfield with big-enough size for `piece`
        bf <- BF.empty (fromIntegral piece + 1)
        BF.set bf (fromIntegral piece)
        atomicModifyIORef_ peer $ \pc' -> pc'{pcPieces=Just bf}
      Just bf ->
        -- just update the bitfield
        BF.set bf (fromIntegral piece)

handleMessage' _ peer Choke =
    atomicModifyIORef_ peer $ \pc -> pc{pcChoking = True}

handleMessage' sess peer Unchoke = do
    pc <- atomicModifyIORef' peer $ \pc -> let pc' = pc{pcChoking = False} in (pc', pc')
    case pcRequest pc of
      Nothing   -> return ()
      Just pIdx -> do
        pmgr <- readMVar $ sessPieceMgr sess
        case pmgr of
          Nothing ->
            warning "Requested a piece and got unchoked without intializing the piece manager."
          Just pmgr' -> do
            missing <- nextMissingPart pmgr' pIdx
            case missing of
              Nothing -> do
                -- somehow this piece is completed, reset the request field
                atomicModifyIORef_ peer $ \pc' ->
                  pc'{pcRequest=if pcRequest pc' == Just pIdx
                     then Nothing
                     else
                       -- peer is updated since the first read, don't
                       -- change it
                       -- TODO: maybe use MVar for peer references too
                       pcRequest pc'}
              Just (pOffset, pLen) ->
                sendPieceRequest peer pIdx pOffset (min pLen $ pcMaxPieceSize pc)

handleMessage' _ peer Interested = do
    atomicModifyIORef_ peer $ \pc -> pc{pcPeerInterested = True}
    -- FIXME: we're just unchoking every peer for now
    unchokePeer peer

handleMessage' _ peer NotInterested =
    atomicModifyIORef_ peer $ \pc -> pc{pcPeerInterested = False}

handleMessage' sess peer (Piece pIdx offset pData) = do
    putStrLn "Got piece response"
    pm <- readMVar $ sessPieceMgr sess
    case pm of
      Nothing     -> warning "Got a piece message before initializing piece manager."
      Just pieces -> do
        newBytes <- writePiece pieces pIdx offset pData
        atomicModifyIORef_ (sessDownloaded sess) (+ fromIntegral newBytes)
        -- request next missing part of the piece
        missing <- nextMissingPart pieces pIdx
        case missing of
          Nothing -> do
            -- piece is complete.
            -- TODO: maybe check the hash here?
            putStrLn "downloaded a piece"
            -- request a new pieces, or call the callback if we're done
            missings <- missingPieces pieces
            case missings of
              [] -> do
                modifyMVar_ (sessRequestedPieces sess) $ return . S.delete pIdx
                atomicModifyIORef_ peer $ \pc -> pc{pcRequest=Nothing}
                cb <- modifyMVar (sessOnTorrentComplete sess) $ \cb -> return (return (), cb)
                cb
              (pIdx' : _) -> do
                modifyMVar_ (sessRequestedPieces sess) $ return . S.insert pIdx' . S.delete pIdx
                atomicModifyIORef_ peer $ \pc -> pc{pcRequest=Just pIdx'}
                nextMissingPart pieces pIdx' >>=
                  \case Nothing -> error $ "Missing pieces returned " ++ show pIdx'
                                             ++ ", but nextMissingPart returned Nothing"
                        Just (pOffset, len) -> do
                          pc <- readIORef peer
                          void $ sendMessage pc $ Request pIdx' pOffset (min len $ pcMaxPieceSize pc)
          Just (pOffset, len) -> do
            pc <- readIORef peer
            void $ sendMessage pc $ Request pIdx pOffset (min len $ pcMaxPieceSize pc)

handleMessage' sess peer (Request pIdx pOffset pLen) = do
    pc <- readIORef peer
    unless (pcPeerChoking pc) $ do
      pm <- readMVar $ sessPieceMgr sess
      case pm of
        Just pm' -> do
          pData <- getPieceData pm' pIdx pOffset pLen
          case pData of
            Nothing ->
              -- we don't have the piece data
              -- TODO: should I send a cancel message or close the
              -- connection?
              return ()
            Just bytes -> do
              void $ sendMessage pc (Piece pIdx pOffset bytes)
              atomicModifyIORef_ (sessUploaded sess) $ \u -> u + fromIntegral (B.length bytes)
        Nothing ->
          -- TODO: should I close the connection?
          return ()

handleMessage' sess peer (Extended (ExtendedHandshake msgTbl msgData hsData)) = do
    putStrLn "Got extended handshake."
    metadataSize <- atomicModifyIORef' peer $ \pc' ->
      let metadataSize = find (\case UtMetadataSize{} -> True
                                     _ -> False) msgData >>= \(UtMetadataSize i) -> return i in
      (pc'{pcExtendedMsgTbl = msgTbl,
           pcMetadataSize   = metadataSize,
           pcClientName     = ehdV hsData,
           pcReqq           = fromMaybe (pcReqq pc') (ehdReqq hsData)},
       metadataSize)
    case metadataSize of
      Nothing -> return ()
      Just s  -> do
        pm <- tryReadMVar $ sessMIPieceMgr sess
        case pm of
          Nothing -> do
            pm' <- newPieceMgr s (2 ^ (14 :: Word32))
            void $ tryPutMVar (sessMIPieceMgr sess) pm'
          Just _ -> return ()

handleMessage' sess peer (Extended (MetadataRequest pIdx)) = do
    miPieces <- tryReadMVar (sessMIPieceMgr sess)
    pc <- readIORef peer
    case miPieces of
      Nothing ->
        -- we don't have metainfo pieces, reject
        void $ sendMessage pc $ Extended $ MetadataReject pIdx
      Just miPieces' -> do
        pieceData <- getPieceData miPieces' pIdx 0 (2 ^ (14 :: Word32))
        case pieceData of
          Nothing ->
            -- we don't have this particular piece, reject
            void $ sendMessage pc $ Extended $ MetadataReject pIdx
          Just pd -> do
            void $ sendMessage pc $ Extended $ MetadataData pIdx (pmTotalSize miPieces') pd

handleMessage' sess peer (Extended (MetadataData pIdx totalSize pData)) = do
    putStrLn "got metadata piece"
    miPieces <- tryReadMVar (sessMIPieceMgr sess)
    miPieces' <- case miPieces of
                   Nothing -> newPieceMgr totalSize (2 ^ (14 :: Word32))
                   Just pm -> return pm
    -- ignoring the return value since we don't keep track of downloaded
    -- bytes of info dictionaries
    _ <- writePiece miPieces' pIdx 0 pData
    -- request another piece
    missings <- missingPieces miPieces'
    case reverse missings of
      (newPIdx : _) -> do
        pc <- readIORef peer
        void $ sendMessage pc $ Extended $ MetadataRequest newPIdx
        atomicModifyIORef_ peer $ \pc' -> pc'{pcRequest=Just newPIdx}
      _ -> do
        atomicModifyIORef_ peer $ \pc' -> pc'{pcRequest=Nothing}
        cb <- modifyMVar (sessOnMIComplete sess) $ \cb -> return (return (), cb)
        cb

handleMessage' _ _ msg = putStrLn $ "Unhandled peer msg: " ++ show msg

unchokePeer :: IORef PeerConn -> IO ()
unchokePeer peer = do
    pc <- atomicModifyIORef' peer $ \pc -> let pc' = pc{pcPeerChoking=False} in (pc', pc')
    void $ sendMessage pc Unchoke

sendInterested :: IORef PeerConn -> Word32 -> IO ()
sendInterested peer pIdx = do
    pc <- atomicModifyIORef' peer $ \pc -> let pc' = pc{pcInterested=True, pcRequest=Just pIdx}
                                           in (pc', pc')
    void $ sendMessage pc Interested

sendPieceRequest :: IORef PeerConn -> Word32 -> Word32 -> Word32 -> IO ()
sendPieceRequest peer pIdx pOffset pLen = do
    pc <- atomicModifyIORef' peer $ \pc -> let pc' = pc{pcRequest=Just pIdx} in (pc', pc')
    void $ sendMessage pc (Request pIdx pOffset pLen)

sendMessage :: PeerConn -> PeerMsg -> IO (Maybe String)
sendMessage PeerConn{pcSock=sock, pcExtendedMsgTbl=tbl} msg =
    case mkPeerMsg tbl msg of
      Left err    -> return $ Just err
      Right bytes -> sendAll sock bytes >> return Nothing

sendMetainfoRequests :: M.Map SockAddr (IORef PeerConn) -> PieceMgr -> IO ()
sendMetainfoRequests peersMap pieces = do
    missings <- missingPieces pieces
    let peerRefs = M.elems peersMap
    peerVals <- mapM readIORef peerRefs
    let peerRefsMap    = M.fromList $ zip peerVals peerRefs
        availablePeers = filter peerFilter peerVals
        asgns          = zip availablePeers missings
    putStrLn $ "assignments: " ++ show asgns
    forM_ asgns $ \(pc, pIdx) -> do
      void $ sendMessage pc $ Extended $ MetadataRequest pIdx
      atomicModifyIORef_ (fromJust $ M.lookup pc peerRefsMap) $ \pc' -> pc'{pcRequest=Just pIdx}
  where
    peerFilter :: PeerConn -> Bool
    peerFilter PeerConn{pcMetadataSize=Just _, pcRequest=Nothing} = True
    peerFilter _                                                  = False

sendPieceRequests :: M.Map SockAddr (IORef PeerConn) -> S.Set Word32 -> PieceMgr -> IO ()
sendPieceRequests peersMap reqs pieces = do
    missings <- ((`S.difference` reqs)  . S.fromList) <$> missingPieces pieces
    putStrLn $ "Missing pieces: " ++ show missings
    let peerRefs = M.elems peersMap
    peerVals <- mapM readIORef peerRefs
    let availablePeers      = filter peerFilter peerVals
        peerRefsMap         = M.fromList $ zip peerVals peerRefs
    availablePeerPieces <- (M.fromList . catMaybes) <$>
      mapM (\p -> case pcPieces p of
                    Nothing -> return Nothing
                    Just ps -> Just . (p,) . S.map fromIntegral <$> BF.availableBits ps) availablePeers
    let asgns               = assignPieces (S.toList missings) availablePeerPieces
    putStrLn $ "assignments: " ++ show asgns
    forM_ asgns $ \(pc, pIdx) -> do
      missingP <- nextMissingPart pieces pIdx
      case missingP of
        Nothing -> return () -- TODO: how can this be?
        Just (pOffset, pSize) -> do
          -- FIXME: this part is ugly.
          let pcRef = fromJust $ M.lookup pc peerRefsMap
          if pcInterested pc
            then sendPieceRequest pcRef pIdx pOffset (min pSize $ pcMaxPieceSize pc)
            else sendInterested pcRef pIdx
  where
    peerFilter :: PeerConn -> Bool
    peerFilter PeerConn{pcInterested=False, pcRequest=Nothing} = True
    peerFilter PeerConn{pcChoking=False, pcRequest=Nothing}    = True
    peerFilter _                                               = False

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

-- * Logging stuff

-- | Logger used in this module.
logger :: String
logger = "Rho.PeerComms.PeerConnection"

warning :: String -> IO ()
warning = L.warningM logger

errorLog :: String -> IO ()
errorLog = L.errorM logger
