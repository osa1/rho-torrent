{-# LANGUAGE NondecreasingIndentation, OverloadedStrings #-}

-- | Communications with peers.
module Rho.PeerComms where

import           Control.Applicative
import           Control.Concurrent
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
    -- ^ torrent that the peer offers
  , pcSock           :: Socket
  } deriving (Show)

data HandshakeStatus
    = HandshakeSent POSIXTime
    | HandshakeEstablished POSIXTime

initPeerCommsHandler :: IO Socket
initPeerCommsHandler = do
    sock <- socket AF_INET Stream defaultProtocol
    bind sock (SockAddrInet (fromIntegral (5433 :: Int)) 0)
    return sock

handshake
  :: SockAddr -> B.ByteString -> B.ByteString
  -> MVar (M.Map SockAddr HandshakeStatus)
  -> IO ()
handshake addr infoHash peerId hss = do
    peerStatus <- takeMVar hss
    ct <- getPOSIXTime
    case M.lookup addr peerStatus of
      Just (HandshakeSent t) -> do
        putStrLn $ "Handshake sent " ++ show (round $ ct - t) ++ " seconds ago. Skipping."
        putMVar hss peerStatus
      Just (HandshakeEstablished t) -> do
        putStrLn $ "Connection established " ++ show (round $ ct - t) ++ " seconds ago. Skipping."
        putMVar hss peerStatus
      Nothing -> do
        putStrLn $ "New peer. Initializing handshake."
        putMVar hss (M.insert addr (HandshakeSent ct) peerStatus)
        sendHandshake addr infoHash peerId

sendHandshake :: SockAddr -> B.ByteString -> B.ByteString -> IO ()
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
    putStrLn $ "Handshake msg length: " ++ show (B.length msg)
    connect sock addr
    putStrLn "connected..."
    sent <- send sock msg
    -- I don't know how a peer is supposed to answer this, just try to read
    -- anything
    contents <- recv sock 1000
    putStrLn $ "Read contents: " ++ show (parseHandshake contents) ++ " from: " ++ show addr
  where
    errHandler :: IOError -> IO ()
    errHandler err@IOError{ioe_type=NoSuchThing} = putStrLn $ "Problems with connection: " ++ show err
    errHandler err@IOError{ioe_type=TimeExpired} = putStrLn $ "Timeout happened: " ++ show err
    errHandler err = putStrLn $ "Unhandled error: " ++ show err

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
