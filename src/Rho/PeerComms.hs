{-# LANGUAGE NondecreasingIndentation, OverloadedStrings #-}

-- | Communications with peers.
module Rho.PeerComms where

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.Chan
import           Control.Monad
import qualified Data.BEncode              as BE
import           Data.Bits
import qualified Data.ByteString           as B
import qualified Data.ByteString.Builder   as BB
import qualified Data.ByteString.Char8     as BC
import qualified Data.ByteString.Lazy      as LB
import qualified Data.Map                  as M
import           Data.Monoid
import           Data.Time.Clock.POSIX     (POSIXTime, getPOSIXTime)
import           Data.Word
import           GHC.IO.Exception
import           Network.Socket            hiding (KeepAlive, recv, recvFrom,
                                            send, sendTo)
import           Network.Socket.ByteString
import           System.IO.Error

import           Rho.Parser
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
  , pcPeerId         :: B.ByteString
  , pcOffers         :: [B.ByteString]
    -- ^ torrents that the peer offers
  , pcSock           :: Socket
    -- ^ socket connected to the peer
  } deriving (Show)

data PeerCommHandler = PeerCommHandler
  { pchPeers :: MVar (M.Map SockAddr PeerConn)
  , pchSock  :: Socket
    -- ^ socket used to listen incoming messages
  }

initPeerCommsHandler :: IO PeerCommHandler
initPeerCommsHandler = do
    peers <- newMVar M.empty
    -- used to pass bytes from socket listener to message handler
    msgChan <- newChan
    sock <- spawnPeerMsgListener msgChan
    async $ peerMsgHandler peers msgChan
    return $ PeerCommHandler peers sock

spawnPeerMsgListener :: Chan (SockAddr, Socket, B.ByteString) -> IO Socket
spawnPeerMsgListener msgChan = do
    sock <- socket AF_INET Stream defaultProtocol
    bind sock (SockAddrInet (fromIntegral (5433 :: Int)) 0) -- TODO: hard-coded port
    async $ peerMsgListener sock
    return sock
  where
    peerMsgListener sock = do
      (peerSocket, peerAddr) <- accept sock
      msg <- recv peerSocket 10000
      writeChan msgChan (peerAddr, peerSocket, msg)
      peerMsgListener sock

peerMsgHandler
    :: MVar (M.Map SockAddr PeerConn)
    -> Chan (SockAddr, Socket, B.ByteString)
    -> IO ()
peerMsgHandler peers msgChan = do
    (peerAddr, peerSock, peerMsg) <- readChan msgChan
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
        case parsePeerMsg peerMsg of
          Nothing -> putStrLn "Can't parse peer msg."
          Just msg -> putStrLn $ "Parsed a peer msg: " ++ show msg
    peerMsgHandler peers msgChan

handshake :: PeerCommHandler -> SockAddr -> B.ByteString -> B.ByteString -> IO ()
handshake PeerCommHandler{pchPeers=peers} addr infoHash peerId = do
    ret <- sendHandshake addr infoHash peerId
    case ret of
      Left err -> putStrLn $ "Can't establish connection: " ++ err
      Right (sock, infoHash, peerId, _msg) -> do
        -- TODO: handle msg
        modifyMVar_ peers $ \peers' ->
          case M.lookup addr peers' of
            Nothing -> do
              -- first time handshaking with the peer
              -- TODO: check info_hash
              putStrLn "Handshake successful"
              return $ M.insert addr (PeerConn True False True False peerId [infoHash] sock) peers'
            Just pc -> do
              -- probably learned about a new torrent
              putStrLn "Handshake successful"
              return $ M.insert addr pc{pcOffers = infoHash : pcOffers pc} peers'

sendHandshake
    :: SockAddr -> B.ByteString -> B.ByteString
    -> IO (Either String (Socket, B.ByteString, B.ByteString, Maybe PeerMsg))
sendHandshake addr infoHash peerId = flip catchIOError errHandler $ do
    sock <- socket AF_INET Stream defaultProtocol
    bind sock (SockAddrInet aNY_PORT 0)
    putStrLn $ "Sending handshake to remote: " ++ show addr
    let msg = LB.toStrict . BB.toLazyByteString . mconcat $
                [ BB.word8 19 -- pstr len: standard for BitTorrent protocol
                , BB.byteString "BitTorrent protocol" -- pstr
                , BB.byteString $ B.pack
                    [0, 0, 0, 0, 0,
                     0 .&. 0x10, -- we support extension protocol
                     0, 0]
                , BB.byteString infoHash
                , BB.byteString peerId
                ]
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

parseHandshake :: B.ByteString -> Either String (B.ByteString, B.ByteString, Maybe PeerMsg)
parseHandshake bs =
    case execParser bs handshakeParser of
      Just ((pstr, infoHash, peerId), rest) -> do
        assert ("Unknown pstr: " ++ BC.unpack pstr) (pstr == "BitTorrent protocol")
        assert ("info_hash length is wrong: " ++ show (B.length infoHash)) (B.length infoHash == 20)
        assert ("peer_id length is wrong: " ++ show (B.length peerId)) (B.length peerId == 20)
        return (infoHash, peerId, parsePeerMsg rest)
      Nothing -> Left "Can't parse handshake message."
  where
    assert :: String -> Bool -> Either String ()
    assert _ True = Right ()
    assert err False = Left err

handshakeParser :: Parser (B.ByteString, B.ByteString, B.ByteString)
handshakeParser = do
    pstrLen <- readWord
    pstr <- replicateM (fromIntegral pstrLen) readWord
    _ <- replicateM 8 readWord
    infoHash <- replicateM 20 readWord
    peerId <- replicateM 20 readWord
    return (B.pack pstr, B.pack infoHash, B.pack peerId)

data PeerMsg
  = KeepAlive
  | Choke
  | Unchoke
  | Interested
  | NotInterested
  | Have Word32
  | Bitfield B.ByteString
  | Request Word32 -- ^ piece index
            Word32 -- ^ offset in piece
            Word32 -- ^ length
  | Piece Word32 -- ^ piece index
          Word32 -- ^ offset in piece
          B.ByteString -- ^ data
  | Cancel Word32 -- ^ piece index
           Word32 -- ^ offset in piece
           Word32 -- ^ length
  | Port PortNumber
  | Extended ExtendedPeerMsg
  deriving (Show)

data ExtendedPeerMsg
  -- Messages from BEP 9
  = MetadataRequest Word32 -- ^ piece index
  | MetadataData
        Word32 -- ^ piece index
        Word64 -- ^ total size, in bytes
        B.ByteString -- ^ data
  | MetadataReject Word32 -- ^ piece index
  deriving (Show)

parsePeerMsg :: B.ByteString -> Maybe PeerMsg
parsePeerMsg bs = fmap fst $ execParser bs $ do
    len <- readWord32
    if len == 0
      then return KeepAlive
      else do
    msgId <- readWord
    case msgId of
      0 -> return Choke
      1 -> return Unchoke
      2 -> return Interested
      3 -> return NotInterested
      4 ->
        -- we know `have` message has fixed, 4-byte payload
        -- (so `len` is always 5)
        -- TODO: maybe make a sanity check here
        Have <$> readWord32
      5 ->
        -- TODO: check for errors
        Bitfield . B.pack <$> replicateM (fromIntegral len - 1) readWord
      6 -> Request <$> readWord32 <*> readWord32 <*> readWord32
      7 -> Piece <$> readWord32 <*> readWord32 <*> consume
      8 -> Cancel <$> readWord32 <*> readWord32 <*> readWord32
      9 -> Port . PortNum <$> readWord16LE
      20 -> Extended <$> parseExtendedPeerMsg len
      _ -> fail $ "Unknown peer message id: " ++ show msgId

parseExtendedPeerMsg :: Word32 -> Parser ExtendedPeerMsg
parseExtendedPeerMsg len = do
    extendedMsgType <- readWord -- could that be anything other than `1`?
    if extendedMsgType == 1
      then do
        -- TODO: redundant bytecode unpacking/packing here?
        payload <- replicateM (fromIntegral $ len - 2) readWord
        bc <- p $ BE.decode (B.pack payload)
        mdict <- p $ getField bc "m"
        case getField mdict "ut_metadata" of
          Left _ -> fail "Can't read ut_metadata"
          Right (BE.BInteger 3) -> parseMsg bc
          Right b -> fail $ "Unknown bencode in ut_metadata: " ++ show b
      else fail $ "Unknown extended message type: " ++ show extendedMsgType
  where
    p :: Either String a -> Parser a
    p = either fail return

    parseMsg :: BE.BValue -> Parser ExtendedPeerMsg
    parseMsg bc = do
      msgType <- p $ getField bc "msg_type" :: Parser Integer
      case msgType of
        0 -> MetadataRequest <$> p (getField bc "piece")
        1 ->
          MetadataData <$> p (getField bc "piece")
                       <*> p (getField bc "total_size")
                       <*> consume
        2 -> MetadataReject <$> p (getField bc "piece")
        _ -> fail $ "Unknown ut_metadata type: " ++ show msgType
