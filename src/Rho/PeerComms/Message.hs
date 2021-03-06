{-# LANGUAGE DeriveAnyClass, DeriveGeneric, NondecreasingIndentation,
             OverloadedStrings, TupleSections #-}

module Rho.PeerComms.Message where

import           Control.DeepSeq         (NFData)
import qualified Data.BEncode            as BE
import qualified Data.BEncode.BDict      as BE
import           Data.Binary.Get
import qualified Data.ByteString         as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy    as LB
import qualified Data.Map                as M
import           Data.Maybe              (catMaybes)
import           Data.Monoid
import           Data.Word
import           GHC.Generics
import           Network.Socket          hiding (KeepAlive)

import           Rho.Instances           ()
import           Rho.Utils

type PieceIdx           = Word32
type PieceOffset        = Word32
type PieceRequestLen    = Word32

data PeerMsg
  = KeepAlive
  | Choke
  | Unchoke
  | Interested
  | NotInterested
  | Have PieceIdx
  | Bitfield B.ByteString
  | Request PieceIdx
            PieceOffset
            PieceRequestLen
  | Piece PieceIdx
          PieceOffset
          B.ByteString -- data
  | Cancel PieceIdx
           PieceOffset
           PieceRequestLen
  | Port PortNumber
  | Extended ExtendedPeerMsg
  deriving (Show, Eq, Generic, NFData)

-- | Extended peer message ids as determined in handshake.
type ExtendedPeerMsgTable = M.Map ExtendedPeerMsgType Word8

data ExtendedPeerMsgType
  = UtMetadata -- ^ BEP 9
  deriving (Show, Eq, Ord, Generic, NFData)

-- | BDict keys of extended peer message types to be used while generating
-- m-dictionaries.
extendedPeerMsgTypeKey :: ExtendedPeerMsgType -> BE.BKey {- = B.ByteString -}
extendedPeerMsgTypeKey UtMetadata = "ut_metadata"

-- | Extended message id we use for ut_metadata.
uT_METADATA_KEY :: Word8
uT_METADATA_KEY = 3

-- | We keep extra data of extensions in handshakes in a separate data
-- type. Ugly, but I don't know any better ways to handle this right now.
data ExtendedMsgTypeData
  = UtMetadataSize Word64
  deriving (Show, Eq, Generic, NFData)

-- | Some widely used fields in extended handshake.
data ExtendedHandshakeData = ExtendedHandshakeData
  { ehdV    :: Maybe B.ByteString -- ^ `v` from BEP 10
  , ehdReqq :: Maybe Word32 -- ^ `reqq` from BEP 10
  } deriving (Show, Eq, Generic, NFData)

-- | Our extended message id table.
defaultMsgTable :: ExtendedPeerMsgTable
defaultMsgTable = M.fromList [(UtMetadata, 3)]

-- | Our extended handshake.
defaultExtendedHs :: Maybe Word64 -> ExtendedPeerMsg
defaultExtendedHs metadataSize =
  ExtendedHandshake defaultMsgTable (catMaybes [fmap UtMetadataSize metadataSize])
                    (ExtendedHandshakeData (Just "rho-torrent 0.1") (Just 128))

data ExtendedPeerMsg
  = ExtendedHandshake
      -- extended message id table parsed from handshake
      ExtendedPeerMsgTable
      -- extra data about extended message types.
      -- (metadata_size from ut_metadata etc.)
      [ExtendedMsgTypeData]
      -- extra data attached to handshake (see BEP 10)
      ExtendedHandshakeData
  | UnknownExtendedMsg Word8
  -- Messages from BEP 9
  | MetadataRequest PieceIdx
  | MetadataData
        PieceIdx
        Word64 -- total size, in bytes
        B.ByteString -- data
  | MetadataReject PieceIdx
  deriving (Show, Eq, Generic, NFData)

mkPeerMsg :: ExtendedPeerMsgTable -> PeerMsg -> Either String B.ByteString
mkPeerMsg msgTbl msg = (LB.toStrict . BB.toLazyByteString . mconcat) <$> mkPeerMsg' msgTbl msg

mkPeerMsg' :: ExtendedPeerMsgTable -> PeerMsg -> Either String [BB.Builder]
mkPeerMsg' _ KeepAlive = return [BB.word32BE 0]
mkPeerMsg' _ Choke = return [BB.word32BE 1, BB.word8 0]
mkPeerMsg' _ Unchoke = return [BB.word32BE 1, BB.word8 1]
mkPeerMsg' _ Interested = return [BB.word32BE 1, BB.word8 2]
mkPeerMsg' _ NotInterested = return [BB.word32BE 1, BB.word8 3]
mkPeerMsg' _ (Have piece) = return [BB.word32BE 5, BB.word8 4, BB.word32BE piece]
mkPeerMsg' _ (Bitfield bf) = return
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
    [BB.word32BE 3, BB.word8 9, BB.word16LE w16]
-- TODO: ut_metadata message generators have duplicate code
mkPeerMsg' _ (Extended (ExtendedHandshake tbl tblData hsData)) =
    let mDictFields, fields :: [(B.ByteString, BE.BValue)]
        mDictFields = map (\(k, v) -> (extendedPeerMsgTypeKey k, BE.toBEncode v)) $ M.toList tbl
        fields      = extendedMsgDataFields tblData ++ hsDataFields hsData
        mDict       = BE.fromAscList mDictFields
        resp        = BE.fromAscList $ ("m", BE.BDict mDict) : fields
        bcString    = LB.toStrict $ BE.encode resp in
    return [ BB.word32BE (fromIntegral $ 2 + B.length bcString)
           , BB.word8 20, BB.word8 0, BB.byteString bcString ]
  where
    extendedMsgDataFields :: [ExtendedMsgTypeData] -> [(B.ByteString, BE.BValue)]
    extendedMsgDataFields = concatMap f
      where
        f :: ExtendedMsgTypeData -> [(B.ByteString, BE.BValue)]
        f (UtMetadataSize s) = [("metadata_size", BE.toBEncode s)]
    hsDataFields :: ExtendedHandshakeData -> [(B.ByteString, BE.BValue)]
    hsDataFields (ExtendedHandshakeData v reqq) =
      maybe [] ((:[]) . ("v",) . BE.toBEncode) v <> maybe [] ((:[]) . ("reqq",) . BE.toBEncode) reqq
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
parsePeerMsg bs = getResult $ flip runGetOrFail (LB.fromStrict bs) $ do
    len <- getWord32be
    if len == 0
      then return KeepAlive
      else do
    msgId <- getWord8
    case msgId of
      0 -> return Choke
      1 -> return Unchoke
      2 -> return Interested
      3 -> return NotInterested
      4 ->
        -- we know `have` message has fixed, 4-byte payload
        -- (so `len` is always 5)
        -- TODO: maybe make a sanity check here
        Have <$> getWord32be
      5 ->
        -- TODO: check for errors
        Bitfield <$> getByteString (fromIntegral len - 1)
      6 -> Request <$> getWord32be <*> getWord32be <*> getWord32be
      7 -> Piece <$> getWord32be <*> getWord32be
                 <*> (LB.toStrict <$> getRemainingLazyByteString)
      8 -> Cancel <$> getWord32be <*> getWord32be <*> getWord32be
      9 -> Port . PortNum <$> getWord16le
      20 -> Extended <$> parseExtendedPeerMsg len
      _ -> fail $ "Unknown peer message id: " ++ show msgId

parseExtendedPeerMsg :: Word32 -> Get ExtendedPeerMsg
parseExtendedPeerMsg len = do
    extendedMsgType <- getWord8
    -- TODO: redundant bytestring unpacking/packing here
    payload <- getByteString (fromIntegral len - 2)
    if extendedMsgType == 0
      then
        case parseExtendedHandshake payload of
          Left err -> fail err
          Right (msgTbl, msgData, hsData) ->
            return $ ExtendedHandshake msgTbl msgData hsData
      else either fail return $ parseExtendedMsg extendedMsgType payload
  where
    opt :: Either String a -> Either String (Maybe a)
    opt (Left _) = return Nothing
    opt (Right a) = return $ Just a

    parseExtendedHandshake
      :: B.ByteString
      -> Either String (ExtendedPeerMsgTable, [ExtendedMsgTypeData], ExtendedHandshakeData)
    parseExtendedHandshake payload = do
      bc <- BE.decode payload
      v <- opt $ getField bc "v"
      reqq <- opt $ getField bc "reqq"
      let hsData = ExtendedHandshakeData v reqq
      mdict <- getField bc "m"
      case getField mdict "ut_metadata" of
        Left _ -> return (M.empty, [], hsData)
        Right (BE.BInteger i) -> do
          -- even though the spec doesn't mention it, `metadata_size` is
          -- actually optional. e.g. when a client who requesting the info
          -- sends this message, it has to omit `metadata_size` because it
          -- doesn't know it.
          let metainfoData = case getField bc "metadata_size" of
                               Left _ -> []
                               Right (BE.BInteger size) -> [UtMetadataSize (fromIntegral size)]
                               Right bv -> fail $ "metadata_size field is not a number: " ++ show bv
          return (M.singleton UtMetadata (fromIntegral i), metainfoData, hsData)
        Right bv -> fail $ "ut_metadata value is not a number: " ++ show bv

    parseExtendedMsg :: Word8 -> B.ByteString -> Either String ExtendedPeerMsg
    parseExtendedMsg t payload
      | t == uT_METADATA_KEY = parseUtMetadataMsg payload
      | otherwise = return $ UnknownExtendedMsg t

    parseUtMetadataMsg :: B.ByteString -> Either String ExtendedPeerMsg
    parseUtMetadataMsg payload = do
      (bc, rest) <- decodeNonConsumed payload
      msgType <- getField bc "msg_type" :: Either String Word8
      case msgType of
        0 -> MetadataRequest <$> getField bc "piece"
        1 -> MetadataData <$> getField bc "piece"
                          <*> getField bc "total_size"
                          <*> pure rest
        2 -> MetadataReject <$> getField bc "piece"
        _ -> fail $ "Unknown ut_metadata type: " ++ show msgType
