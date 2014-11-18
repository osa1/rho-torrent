{-# LANGUAGE NondecreasingIndentation, OverloadedStrings #-}

module Rho.PeerComms.Message
  ( PeerMsg (..)
  , ExtendedPeerMsg (..)
  , mkPeerMsg
  , parsePeerMsg
  ) where

import           Control.Applicative
import           Control.Monad
import qualified Data.BEncode            as BE
import qualified Data.ByteString         as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy    as LB
import           Data.Monoid
import           Data.Word
import           Network.Socket          hiding (KeepAlive)

import qualified Rho.Bitfield            as BF
import           Rho.Parser
import           Rho.Utils

data PeerMsg
  = KeepAlive
  | Choke
  | Unchoke
  | Interested
  | NotInterested
  | Have Word32
  | Bitfield BF.Bitfield
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
  deriving (Show, Eq)

data ExtendedPeerMsg
  -- Messages from BEP 9
  = MetadataRequest Word32 -- ^ piece index
  | MetadataData
        Word32 -- ^ piece index
        Word64 -- ^ total size, in bytes
        B.ByteString -- ^ data
  | MetadataReject Word32 -- ^ piece index
  deriving (Show, Eq)

mkPeerMsg :: PeerMsg -> B.ByteString
mkPeerMsg = LB.toStrict . BB.toLazyByteString . mconcat . mkPeerMsg'

mkPeerMsg' :: PeerMsg -> [BB.Builder]
mkPeerMsg' KeepAlive = [BB.word32BE 0]
mkPeerMsg' Choke = [BB.word32BE 1, BB.word8 0]
mkPeerMsg' Unchoke = [BB.word32BE 1, BB.word8 1]
mkPeerMsg' Interested = [BB.word32BE 1, BB.word8 2]
mkPeerMsg' NotInterested = [BB.word32BE 1, BB.word8 3]
mkPeerMsg' (Have piece) = [BB.word32BE 5, BB.word8 4, BB.word32BE piece]
mkPeerMsg' (Bitfield (BF.Bitfield bf)) =
    [ BB.word32BE (fromIntegral $ 1 + B.length bf)
    , BB.word8 5
    , BB.byteString bf ]
mkPeerMsg' (Request pidx offset len) =
    [BB.word32BE 13, BB.word8 6, BB.word32BE pidx, BB.word32BE offset, BB.word32BE len]
mkPeerMsg' (Piece pidx offset piece) =
    [ BB.word32BE (fromIntegral $ 9 + B.length piece)
    , BB.word8 7
    , BB.word32BE pidx
    , BB.word32BE offset
    , BB.byteString piece ]
mkPeerMsg' (Cancel pidx offset len) =
    [BB.word32BE 13, BB.word8 8, BB.word32BE pidx, BB.word32BE offset, BB.word32BE len]
mkPeerMsg' (Port (PortNum w16)) =
    [BB.word32BE 3, BB.word8 9, BB.word16LE w16 {- TODO: not sure about endianness of port -}]
-- TODO: implement extensions

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
        Bitfield . BF.Bitfield . B.pack <$> replicateM (fromIntegral len - 1) readWord
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
