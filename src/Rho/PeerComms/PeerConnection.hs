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
import           Network.Socket              hiding (KeepAlive, recv, recvFrom,
                                              recvLen, send, sendTo)
import           Network.Socket.ByteString
import           System.IO.Error
import qualified System.Log.Logger           as L

import qualified Rho.Bitfield                as BF
import           Rho.Listener                (Listener, recvLen)
import           Rho.PeerComms.Handshake
import           Rho.PeerComms.Message
import           Rho.PeerComms.PeerConnState
import           Rho.PeerComms.PeerPieceAsgn
import           Rho.PieceMgr
import           Rho.Utils

-- | Listen a connected socket and handle incoming messages.
listenConnectedSock :: IORef PeerConn -> PieceMgr -> Listener -> Socket -> SockAddr -> IO ()
listenConnectedSock peer pieces listener sock sockAddr = flip catchIOError errHandler $ loop
  where
    loop = do
      msg <- recvMessage listener
      case msg of
        ConnClosed msg
          | B.null msg -> return ()
          | otherwise  -> putStrLn ("recvd a partial message: " ++ show (B.unpack msg)) >> return ()
        Msg msg -> handleMessage peer pieces msg sock sockAddr >> loop

    errHandler err = do
      putStrLn $ "Error happened while listening a socket: " ++ show err
                  ++ ". Closing the connection."

handleMessage :: IORef PeerConn -> PieceMgr -> B.ByteString -> Socket -> SockAddr -> IO ()
handleMessage peer pieces msg _sock peerAddr = do
    case parsePeerMsg msg of
      Left err -> warning . concat $
        [ "Can't parse peer message: ", err,
          " msg: ", show msg, " msg length: ", show (B.length msg) ]
      Right KeepAlive -> return () -- TODO: should I ignore keep-alives?
      Right (Bitfield (BF.Bitfield bytes _)) ->
        -- we ignore parsed length and use number of pieces that we know
        -- FIXME: we need to check piece index in have message before
        -- calling `Bitfield.set` to prevent `error`.
        atomicModifyIORef' peer $ \pc ->
          (pc{pcPieces = Just (BF.Bitfield bytes $ fromIntegral $ pmPieces pieces)}, ())
      Right (Have piece) ->
        atomicModifyIORef' peer $ \pc ->
          let bf' = Just $ BF.set (fromMaybe (BF.empty $ fromIntegral $ pmPieces pieces) $ pcPieces pc)
                                  (fromIntegral piece)
          in (pc{pcPieces = bf'}, ())
      Right Choke -> atomicModifyIORef' peer $ \pc -> (pc{pcChoking = True}, ())
      Right Unchoke -> atomicModifyIORef' peer $ \pc -> (pc{pcChoking = False}, ())
      Right Interested -> atomicModifyIORef' peer $ \pc -> (pc{pcPeerInterested = True}, ())
      Right NotInterested -> atomicModifyIORef' peer $ \pc -> (pc{pcPeerInterested = False}, ())
      Right (Piece pIdx offset pData) -> do
        putStrLn "Got piece response"
        writePiece pieces pIdx offset pData
      Right (Extended (ExtendedHandshake msgTbl msgData hsData)) -> do
        pc <- readIORef peer
        putStrLn "Got extended handshake."
        -- TODO: We should send extended handshake before receiving one
        sendMessage pc (Extended $ defaultExtendedHS Nothing) -- FIXME: info size
        atomicModifyIORef' peer $ \pc' ->
          (pc'{pcExtendedMsgTbl = msgTbl,
               pcMetadataSize   = find (\case UtMetadataSize s -> True
                                              _ -> False) msgData >>= \(UtMetadataSize i) -> return i,
               pcClientName     = ehdV hsData,
               pcReqq           = fromMaybe (pcReqq pc') (ehdReqq hsData)},
           ())
      Right pmsg -> putStrLn $ "Unhandled peer msg: " ++ show pmsg

sendMessage :: PeerConn -> PeerMsg -> IO (Maybe String)
sendMessage PeerConn{pcSock=sock, pcExtendedMsgTbl=tbl} msg =
    case mkPeerMsg tbl msg of
      Left err -> return $ Just err
      Right bytes -> send sock bytes >> return Nothing

sendPieceRequests :: MVar (M.Map SockAddr (IORef PeerConn)) -> PieceMgr -> IO ()
sendPieceRequests peers pieces = do
    -- TODO: fix horrible piece request algortihm
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
