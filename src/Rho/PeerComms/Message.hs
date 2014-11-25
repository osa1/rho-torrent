{-# LANGUAGE NondecreasingIndentation, OverloadedStrings #-}

module Rho.PeerComms.Message where

import           Control.Applicative
import           Control.Monad
import qualified Data.BEncode            as BE
import qualified Data.BEncode.BDict      as BE
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
  | Request Word32 -- piece index
            Word32 -- offset in piece
            Word32 -- length
  | Piece Word32 -- piece index
          Word32 -- offset in piece
          B.ByteString -- data
  | Cancel Word32 -- piece index
           Word32 -- offset in piece
           Word32 -- length
  | Port PortNumber
  | Extended ExtendedPeerMsg
  deriving (Show, Eq)

-- | Extended peer message ids as determined in handshake.
type ExtendedPeerMsgTable = M.Map ExtendedPeerMsgType Word8

data ExtendedPeerMsgType
  = UtMetadata -- ^ BEP 9
  deriving (Show, Eq, Ord)

-- | BDict keys of extended peer message types to be used while generating
-- m-dictionaries.
extendedPeerMsgTypeKey :: ExtendedPeerMsgType -> BE.BKey {- = B.ByteString -}
extendedPeerMsgTypeKey UtMetadata = "ut_metadata"

uT_METADATA_KEY :: Word8
uT_METADATA_KEY = 3

-- | We keep extra data of extensions in handshakes in a separate data
-- type. Ugly, but I don't know any better ways to handle this right now.
data ExtendedMsgTypeData
  = UtMetadataSize Word32
  deriving (Show, Eq)

defaultMsgTable :: ExtendedPeerMsgTable
defaultMsgTable = M.fromList [(UtMetadata, 3)]

data ExtendedPeerMsg
  = ExtendedHandshake
      -- extended message id table parsed from handshake
      ExtendedPeerMsgTable
      -- extra data about extended message types.
      -- (metadata_size from ut_metadata etc.)
      [ExtendedMsgTypeData]
  | UnknownExtendedMsg Word8
  -- Messages from BEP 9
  | MetadataRequest Word32 -- piece index
  | MetadataData
        Word32 -- piece index
        Word64 -- total size, in bytes
        B.ByteString -- data
  | MetadataReject Word32 -- piece index
  deriving (Show, Eq)

mkPeerMsg :: ExtendedPeerMsgTable -> PeerMsg -> Either String B.ByteString
mkPeerMsg msgTbl msg = (LB.toStrict . BB.toLazyByteString . mconcat) <$> mkPeerMsg' msgTbl msg

mkPeerMsg' :: ExtendedPeerMsgTable -> PeerMsg -> Either String [BB.Builder]
mkPeerMsg' _ KeepAlive = return [BB.word32BE 0]
mkPeerMsg' _ Choke = return [BB.word32BE 1, BB.word8 0]
mkPeerMsg' _ Unchoke = return [BB.word32BE 1, BB.word8 1]
mkPeerMsg' _ Interested = return [BB.word32BE 1, BB.word8 2]
mkPeerMsg' _ NotInterested = return [BB.word32BE 1, BB.word8 3]
mkPeerMsg' _ (Have piece) = return [BB.word32BE 5, BB.word8 4, BB.word32BE piece]
mkPeerMsg' _ (Bitfield (BF.Bitfield bf)) = return
    [ BB.word32BE (fromIntegral $ 1 + B.length bf)
    , BB.word8 5
    , BB.byteString bf ]
mkPeerMsg' _ (Request pidx offset len) = return
    [BB.word32BE 13, BB.word8 6, BB.word32BE pidx, BB.word32BE offset, BB.word32BE len]
mkPeerMsg' _ (Piece pidx offset piece) = return
    [ BB.word32BE (fromIntegral $ 9 + B.length piece)
    , BB.word8 7
    , BB.word32BE pidx
    , BB.word32BE offset
    , BB.byteString piece ]
mkPeerMsg' _ (Cancel pidx offset len) = return
    [BB.word32BE 13, BB.word8 8, BB.word32BE pidx, BB.word32BE offset, BB.word32BE len]
mkPeerMsg' _ (Port (PortNum w16)) = return
    [BB.word32BE 3, BB.word8 9, BB.word16LE w16 {- TODO: not sure about endianness of port -}]
-- TODO: ut_metadata message generators have duplicate code
mkPeerMsg' _ (Extended (ExtendedHandshake tbl tblData)) =
    let mDictFields :: [(B.ByteString, BE.BValue)]
        mDictFields = map (\(k, v) -> (extendedPeerMsgTypeKey k, BE.toBEncode v)) $ M.toList tbl
        fields      :: [(B.ByteString, BE.BValue)]
        fields      = extendedMsgDataFields tblData
        -- TODO: sort fields
        mDict           = BE.fromAscList mDictFields
        resp            = BE.fromAscList $ ("m", BE.BDict mDict) : fields
        bcString        = LB.toStrict $ BE.encode resp in
    return [ BB.word32BE (fromIntegral $ 2 + B.length bcString)
           , BB.word8 20, BB.word8 0, BB.byteString bcString ]
  where
    extendedMsgDataFields :: [ExtendedMsgTypeData] -> [(B.ByteString, BE.BValue)]
    extendedMsgDataFields = concatMap f
      where
        f :: ExtendedMsgTypeData -> [(B.ByteString, BE.BValue)]
        f (UtMetadataSize s) = [("metadata_size", BE.toBEncode s)]
mkPeerMsg' _ (Extended UnknownExtendedMsg{}) = Left "Can't generate message for UnknownExtendedMsg"
mkPeerMsg' tbl (Extended (MetadataRequest pidx)) =
    case M.lookup UtMetadata tbl of
      Nothing -> Left "Can't find ut_metadata id in peer message id table."
      Just i  ->
        let bcString = LB.toStrict $ BE.encode $ BE.fromAscList
                         [ ("msg_type", BE.BInteger 0), ("piece", BE.toBEncode pidx) ] in
        return [ BB.word32BE (fromIntegral $ 2 + B.length bcString)
               , BB.word8 20, BB.word8 i
               , BB.byteString bcString ]
mkPeerMsg' tbl (Extended (MetadataData pidx totalSize piece)) =
    case M.lookup UtMetadata tbl of
      Nothing -> Left "Can't find ut_metadata id in peer message id table."
      Just i  ->
        let bcString = LB.toStrict $ BE.encode $ BE.fromAscList
                         [ ("msg_type", BE.BInteger 1)
                         , ("piece", BE.toBEncode pidx)
                         , ("total_size", BE.toBEncode totalSize) ] in
        return [ BB.word32BE (fromIntegral $ 2 + B.length bcString + B.length piece)
               , BB.word8 20, BB.word8 i
               , BB.byteString bcString
               , BB.byteString piece ]
mkPeerMsg' tbl (Extended (MetadataReject pidx)) =
    case M.lookup UtMetadata tbl of
      Nothing -> Left "Can't find ut_metadata id in peer message id table."
      Just i  ->
        let bcString = LB.toStrict $ BE.encode $ BE.fromAscList
                         [ ("msg_type", BE.BInteger 2) , ("piece", BE.toBEncode pidx) ] in
        return [ BB.word32BE (fromIntegral $ 2 + B.length bcString)
               , BB.word8 20, BB.word8 i
               , BB.byteString bcString ]

parsePeerMsg :: B.ByteString -> Either String PeerMsg
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
    extendedMsgType <- readWord
    -- TODO: redundant bytestring unpacking/packing here
    payload <- B.pack <$> replicateM (fromIntegral len - 2) readWord
    if extendedMsgType == 0
      then uncurry ExtendedHandshake <$> parseExtendedHandshake payload
      else parseExtendedMsg extendedMsgType payload
  where
    p :: Either String a -> Parser a
    p = either fail return

    parseExtendedHandshake :: B.ByteString -> Parser (ExtendedPeerMsgTable, [ExtendedMsgTypeData])
    parseExtendedHandshake payload = do
      bc <- p $ BE.decode payload
      mdict <- p $ getField bc "m"
      case getField mdict "ut_metadata" of
        Left _ -> return (M.empty, [])
        Right (BE.BInteger i) -> do
          -- even though the spec doesn't mention it, `metadata_size` is
          -- actually optinal. e.g. when a client who requesting the info
          -- sends this message, it has to omit `metadata_size` because it
          -- doesn't know it.
          let metainfoData = case getField bc "metadata_size" of
                               Left _ -> []
                               Right (BE.BInteger size) -> [UtMetadataSize (fromIntegral size)]
                               Right _ -> [] -- TODO: error?
          return (M.singleton UtMetadata (fromIntegral i), metainfoData)
        Right bv -> fail $ "ut_metadata value is not a number: " ++ show bv

    parseExtendedMsg :: Word8 -> B.ByteString -> Parser ExtendedPeerMsg
    parseExtendedMsg t payload
      | t == uT_METADATA_KEY = parseUtMetadataMsg payload
      | otherwise = return $ UnknownExtendedMsg t

    parseUtMetadataMsg :: B.ByteString -> Parser ExtendedPeerMsg
    parseUtMetadataMsg payload = do
      (bc, rest) <- p $ decodeNonConsumed payload
      msgType <- p $ getField bc "msg_type" :: Parser Word8
      case msgType of
        0 -> MetadataRequest <$> p (getField bc "piece")
        1 -> MetadataData <$> p (getField bc "piece")
                          <*> p (getField bc "total_size")
                          <*> pure rest
        2 -> MetadataReject <$> p (getField bc "piece")
        _ -> fail $ "Unknown ut_metadata type: " ++ show msgType
