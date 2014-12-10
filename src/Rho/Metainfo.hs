{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, FlexibleInstances,
             OverloadedStrings, RecordWildCards #-}

module Rho.Metainfo where

import           Control.Applicative
import           Data.BEncode            as BE
import qualified Data.BEncode.BDict      as BE
import qualified Data.ByteString         as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy    as LB
import           Data.Digest.SHA1
import           Data.Maybe              (fromMaybe)
import           Data.Monoid
import           Data.Typeable
import           Data.Word
import           GHC.Generics

import           Rho.InfoHash
import           Rho.Tracker

-- | Metainfo file as specified in
-- https://wiki.theory.org/BitTorrentSpecification#Metainfo_File_Structure
data Metainfo = Metainfo
  { mAnnounce     :: Tracker
  , mAnnounceList :: Maybe [[Tracker]]
  , mCreationDate :: Maybe Int
  , mComment      :: Maybe B.ByteString
  , mCreatedBy    :: Maybe B.ByteString
  , mEncoding     :: Maybe B.ByteString
  , mInfoHash     :: InfoHash
  , mInfo         :: Info
  } deriving (Show, Eq, Typeable, Generic)

data Info = Info
  { iName        :: B.ByteString
  , iPieceLength :: Word32
  , iPieces      :: [B.ByteString]
  , iPrivate     :: Bool
  , iFiles       :: Either File [File]
    -- ^ Left: single-file mode, Right: multi-file mode
  } deriving (Show, Eq, Typeable, Generic)

torrentSize :: Info -> Word64
torrentSize Info{iFiles=Left (File len _ _)} = len
torrentSize Info{iFiles=Right files} = sum $ map fLength files

data File = File
  { fLength :: Word64
  , fMd5Sum :: Maybe B.ByteString
  , fPath   :: [B.ByteString]
    -- ^ empty in single-file mode
  } deriving (Show, Eq, Typeable, Generic)

-- | Parse contents of a .torrent file.
parseMetainfo :: B.ByteString -> Either String Metainfo
parseMetainfo = decode

printMetainfo :: Metainfo -> B.ByteString
printMetainfo = LB.toStrict . encode

-- * BEncode instances

instance BEncode Metainfo where
  toBEncode Metainfo{..} = toDict $
       "announce"      .=! mAnnounce
    .: "announce-list" .=? mAnnounceList
    .: "creation date" .=? mCreationDate
    .: "comment"       .=? mComment
    .: "created by"    .=? mCreatedBy
    .: "encoding"      .=? mEncoding
    .: "info"          .=! mInfo
    .: endDict

  fromBEncode = fromDict $ do
    announce <- field $ req "announce"
    announceList <- optional $ field $ req "announce-list"
    creation <- optional $ field $ req "creation date"
    comment <- optional $ field $ req "comment"
    createdBy <- optional $ field $ req "created by"
    encoding <- optional $ field $ req "encoding"
    infoDict <- field $ req "info"
    let infoHash = mkInfoHash infoDict
    case fromBEncode infoDict of
      Left err -> fail err
      Right info ->
        return $ Metainfo announce announceList creation comment createdBy encoding infoHash info

-- | (20-byte) SHA1 hash of info field.
-- We're just hoping that no information is lost/chaged during bencode
-- decoding -> encoding.
mkInfoHash :: BValue -> InfoHash
mkInfoHash bv = InfoHash . hashToBS . hash . LB.unpack . encode $ bv
  where
    -- | Convert 20-byte hash to big-endian bytestring.
    hashToBS :: Word160 -> B.ByteString
    hashToBS (Word160 w1 w2 w3 w4 w5) =
      LB.toStrict . BB.toLazyByteString . mconcat . map BB.word32BE $ [w1, w2, w3, w4, w5]

instance BEncode Info where
  toBEncode (Info name pl ps priv (Right files)) = toDict $
       "files"        .=! files
    .: "name"         .=! name
    .: "piece length" .=! pl
    .: "pieces"       .=! B.concat ps
    .: "private"      .=? (if priv then Just True else Nothing)
    .: endDict

  toBEncode (Info name pl ps priv (Left (File flen md5 _))) = toDict $
       "length"       .=! flen
    .: "md5sum"       .=? md5
    .: "name"         .=! name
    .: "piece length" .=! pl
    .: "pieces"       .=! B.concat ps
    .: "private"      .=? (if priv then Just True else Nothing)
    .: endDict

  fromBEncode bv@(BDict (BE.Cons key _ _))
    | key == "length" = flip fromDict bv $ do
        -- first key is "length", single file mode
        flen <- field $ req "length"
        md5sum <- optional $ field $ req "md5sum"
        name <- field $ req "name"
        pieceLength <- field $ req "piece length"
        pieces <- splitPieces <$> field (req "pieces")
        private <- readPrivate
        return $ Info name pieceLength pieces private (Left $ File flen md5sum [])
    | otherwise = flip fromDict bv $ do
        -- multi file mode
        files <- field $ req "files"
        name <- field $ req "name"
        pieceLength <- field $ req "piece length"
        pieces <- splitPieces <$> field (req "pieces")
        private <- readPrivate
        return $ Info name pieceLength pieces private (Right $ files)
    where
      readPrivate :: Get Bool
      readPrivate = fromMaybe False <$> optional (field $ req "private")

      splitPieces :: B.ByteString -> [B.ByteString]
      splitPieces bs
        | B.length bs < 20 = [] -- TODO: This doesn't look right .. I did this to make testing easier
        | otherwise = let (h, t) = B.splitAt 20 bs in h : splitPieces t
  fromBEncode bv = Left $ "Can't parse info dict from " ++ show bv

instance BEncode File where
  toBEncode File{..} = toDict $
       "length" .=! fLength
    .: "md5sum" .=? fMd5Sum
    .: "path"   .=! fPath
    .: endDict

  fromBEncode = fromDict $ File <$>! "length" <*>? "md5sum" <*>! "path"
