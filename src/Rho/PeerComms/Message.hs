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
import qualified Data.Map                as M
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

-- | Extended peer message ids as determined in handshake.
type ExtendedPeerMsgTable = M.Map Word8 ExtendedPeerMsgType

data ExtendedPeerMsgType
  = UtMetadata Word32 -- ^ BEP 9, size of metadata
  deriving (Show, Eq)

data ExtendedPeerMsg
  = ExtendedHandshake ExtendedPeerMsgTable
  | UnknownExtendedMsg Word8
  -- Messages from BEP 9
  | MetadataRequest Word32 -- ^ piece index
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

parsePeerMsg :: ExtendedPeerMsgTable -> B.ByteString -> Maybe PeerMsg
parsePeerMsg msgTable bs = fmap fst $ execParser bs $ do
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
      20 -> Extended <$> parseExtendedPeerMsg msgTable len
      _ -> fail $ "Unknown peer message id: " ++ show msgId

parseExtendedPeerMsg :: ExtendedPeerMsgTable -> Word32 -> Parser ExtendedPeerMsg
parseExtendedPeerMsg extendedMsgTable len = do
    extendedMsgType <- readWord
    -- TODO: redundant bytestring unpacking/packing here
    payload <- B.pack <$> replicateM (fromIntegral len - 2) readWord
    if extendedMsgType == 0
      then ExtendedHandshake <$> parseExtendedHandshake payload
      else parseExtendedMsg extendedMsgType payload
  where
    p :: Either String a -> Parser a
    p = either fail return

    parseExtendedHandshake :: B.ByteString -> Parser ExtendedPeerMsgTable
    parseExtendedHandshake payload = do
      bc <- p $ BE.decode payload
      mdict <- p $ getField bc "m"
      case getField mdict "ut_metadata" of
        Left _ -> return M.empty
        Right (BE.BInteger i) -> do
          size <- p $ getField bc "metadata_size" :: Parser Word32
          return $ M.singleton (fromIntegral i) (UtMetadata size)
        Right bv -> fail $ "ut_metadata value is not a number: " ++ show bv

    parseExtendedMsg :: Word8 -> B.ByteString -> Parser ExtendedPeerMsg
    parseExtendedMsg t payload =
      case M.lookup t extendedMsgTable of
        Nothing -> return $ UnknownExtendedMsg t
        Just (UtMetadata _) -> parseUtMetadataMsg payload

    parseUtMetadataMsg :: B.ByteString -> Parser ExtendedPeerMsg
    parseUtMetadataMsg payload = do
      bc <- p $ BE.decode payload
      msgType <- p $ getField bc "msg_type" :: Parser Word8
      case msgType of
        0 -> MetadataRequest <$> p (getField bc "piece")
        1 -> MetadataData <$> p (getField bc "piece")
                          <*> p (getField bc "total_size")
                          <*> consume
        2 -> MetadataReject <$> p (getField bc "piece")
        _ -> fail $ "Unknown ut_metadata type: " ++ show msgType
