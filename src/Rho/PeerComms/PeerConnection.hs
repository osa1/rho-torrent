{-# LANGUAGE LambdaCase, MultiWayIf, NondecreasingIndentation, OverloadedStrings
             #-}

-- | Handling communications with peers after a successful handshake.
module Rho.PeerComms.PeerConnection where

import           Control.Concurrent
import           Control.Monad
import qualified Data.ByteString             as B
import           Data.IORef
import           Data.List                   (find)
import qualified Data.Map                    as M
import           Data.Maybe
import           Data.Monoid
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
listenConnectedSock sess peer listener = flip catchIOError errHandler $ loop
  where
    loop = do
      msg <- recvMessage listener
      case msg of
        ConnClosed msg'
          | B.null msg' -> return ()
          | otherwise  -> putStrLn ("recvd a partial message: " ++ show (B.unpack msg')) >> return ()
        Msg msg' -> handleMessage sess peer msg' >> yield >> loop

    errHandler err = do
      putStrLn $ "Error happened while listening a socket: " ++ show err
                  ++ ". Closing the connection."

handleMessage :: Session -> IORef PeerConn -> B.ByteString -> IO ()
handleMessage sess peer msg = do
    case parsePeerMsg msg of
      Left err -> warning . concat $
        [ "Can't parse peer message: ", err,
          " msg: ", show msg, " msg length: ", show (B.length msg) ]
      Right KeepAlive -> return () -- TODO: should I ignore keep-alives?
      Right (Bitfield bf@(BF.Bitfield bytes _)) -> do
        -- FIXME: we need to check piece index in have message before
        -- calling `Bitfield.set` to prevent `error`.
        pm <- takeMVar $ sessPieceMgr sess
        case pm of
          Nothing ->
            -- we don't know how many pieces we have yet, just set it using
            -- parsed bitfield
            atomicModifyIORef' peer $ \pc -> (pc{pcPieces = Just bf}, ())
          Just pm' ->
            -- TODO: Check spare bits and close the connection if they're
            -- not 0
            atomicModifyIORef' peer $ \pc ->
              (pc{pcPieces = Just (BF.Bitfield bytes $ fromIntegral $ pmPieces pm')}, ())
        putMVar (sessPieceMgr sess) pm
      Right (Have piece) ->
        atomicModifyIORef' peer $ \pc ->
          case pcPieces pc of
            Nothing ->
              -- we need to initialize bitfield with big-enough size for `piece`
              let bf = BF.set (BF.empty (fromIntegral piece + 1)) (fromIntegral piece) in
              (pc{pcPieces=Just bf}, ())
            Just bf ->
              -- just update the bitfield
              (pc{pcPieces=Just (BF.set bf (fromIntegral piece))}, ())
      Right Choke -> atomicModifyIORef' peer $ \pc -> (pc{pcChoking = True}, ())
      Right Unchoke -> atomicModifyIORef' peer $ \pc -> (pc{pcChoking = False}, ())
      Right Interested -> atomicModifyIORef' peer $ \pc -> (pc{pcPeerInterested = True}, ())
      Right NotInterested -> atomicModifyIORef' peer $ \pc -> (pc{pcPeerInterested = False}, ())
      Right (Piece pIdx offset pData) -> do
        putStrLn "Got piece response"
        pm <- readMVar $ sessPieceMgr sess
        case pm of
          Nothing -> warning "Got a piece message before initializing piece manager."
          Just pieces -> writePiece pieces pIdx offset pData
      Right (Extended (ExtendedHandshake msgTbl msgData hsData)) -> do
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
            pm <- takeMVar $ sessMIPieceMgr sess
            case pm of
              Nothing -> do
                pm' <- newPieceMgr s (2 ^ (14 :: Word32))
                putMVar (sessMIPieceMgr sess) (Just pm')
              Just _ -> putMVar (sessMIPieceMgr sess) pm
      Right (Extended (MetadataRequest pIdx)) -> do
        miPieces <- readMVar (sessMIPieceMgr sess)
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
      Right (Extended (MetadataData pIdx totalSize pData)) -> do
        putStrLn "got metadata piece"
        miPieces <- readMVar (sessMIPieceMgr sess)
        miPieces' <- case miPieces of
                       Nothing -> newPieceMgr totalSize (2 ^ (14 :: Word32))
                       Just pm -> return pm
        -- TODO: what happens if we already have the piece?
        writePiece miPieces' pIdx 0 pData
        -- request another piece
        missings <- missingPieces miPieces'
        case reverse missings of
          ((newPIdx, _, _) : _) -> do
            pc <- readIORef peer
            void $ sendMessage pc $ Extended $ MetadataRequest newPIdx
          _ -> return ()
      Right pmsg -> putStrLn $ "Unhandled peer msg: " ++ show pmsg

sendMessage :: PeerConn -> PeerMsg -> IO (Maybe String)
sendMessage PeerConn{pcSock=sock, pcExtendedMsgTbl=tbl} msg =
    case mkPeerMsg tbl msg of
      Left err    -> return $ Just err
      Right bytes -> sendAll sock bytes >> return Nothing

sendMetainfoRequests :: MVar (M.Map SockAddr (IORef PeerConn)) -> PieceMgr -> IO ()
sendMetainfoRequests peers pieces = do
    missings <- missingPieces pieces
    availablePeers <- readMVar peers >>= fmap (filter peerFilter) . mapM readIORef . M.elems
    let mips  = map (\(pIdx, _, _) -> pIdx) missings
        asgns = zip availablePeers mips
    putStrLn $ "assignments: " ++ show asgns
    forM_ asgns $ \(pc, pIdx) ->
      sendMessage pc $ Extended $ MetadataRequest pIdx
  where
    peerFilter :: PeerConn -> Bool
    peerFilter PeerConn{pcMetadataSize=Just _} = True
    peerFilter _                               = False

sendPieceRequests :: MVar (M.Map SockAddr (IORef PeerConn)) -> PieceMgr -> IO ()
sendPieceRequests peers pieces = do
    missings <- missingPieces pieces
    putStrLn $ "Missing pieces: " ++ show missings
    availablePeers <- readMVar peers >>= fmap (filter $ not . pcChoking) . mapM readIORef . M.elems
    let availablePeerPieces =
          M.fromList $ mapMaybe (\p -> case pcPieces p of
                                         Nothing -> Nothing
                                         Just ps -> Just (p, BF.availableBits ps)) availablePeers
        asgns = assignPieces missings availablePeerPieces
    putStrLn $ "assignments: " ++ show asgns
    forM_ asgns $ \(pc, (pIdx, pOffset, pSize)) ->
      sendMessage pc $ Request pIdx pOffset (min pSize $ pcMaxPieceSize pc)

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
