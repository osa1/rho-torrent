{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, FlexibleInstances,
             OverloadedStrings, RecordWildCards, TemplateHaskell #-}

module Rho.Metainfo where

import           Control.Applicative
import           Data.BEncode            as BE
import qualified Data.BEncode.BDict      as BE
import qualified Data.ByteString         as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy    as LB
import           Data.Digest.SHA1
import           Data.Monoid
import           Data.Typeable
import           GHC.Generics

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
  , mInfo         :: Info
  } deriving (Show, Typeable, Generic)

data Info = Info
  { iName        :: B.ByteString
  , iPieceLength :: Int
  , iPieces      :: [B.ByteString]
  , iPrivate     :: Bool
  , iFiles       :: [File]
  , iHash        :: B.ByteString
  } deriving (Show, Eq, Typeable, Generic)

data File = File
  { fLength :: Int
  , fMd5Sum :: Maybe B.ByteString
  , fPath   :: [B.ByteString]
  } deriving (Show, Eq, Typeable, Generic)

-- | Parse contents of a .torrent file.
parseMetainfo :: B.ByteString -> Either String Metainfo
parseMetainfo = decode

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

  fromBEncode = fromDict $
    Metainfo <$>! "announce"
             <*>? "announce-list"
             <*>? "creation date"
             <*>? "comment"
             <*>? "created by"
             <*>? "encoding"
             <*>! "info"

instance BEncode Info where
  toBEncode i = toDict $
       "files"        .=! iFiles i
    .: "name"         .=! iName i
    .: "piece length" .=! iPieceLength i
    .: "pieces"       .=! B.concat (iPieces i)
    .: "private"      .=? (if iPrivate i then Just True else Nothing)
    .: endDict

  fromBEncode bv@(BDict (BE.Cons key _ _))
    | key == "length" = flip fromDict bv $ do
        -- first key is "length", single file mode
        flen <- field $ req "length"
        md5sum <- optional $ field $ req "md5sum"
        name <- field $ req "name"
        pieceLength <- field $ req "piece length"
        pieces <- splitPieces <$> (field $ req "pieces")
        private <- readPrivate
        return $ Info name pieceLength pieces private [File flen md5sum []] infoHash
    | otherwise = flip fromDict bv $ do
        -- multi file mode
        files <- field $ req "files"
        name <- field $ req "name"
        pieceLength <- field $ req "piece length"
        pieces <- splitPieces <$> (field $ req "pieces")
        private <- readPrivate
        return $ Info name pieceLength pieces private files infoHash
    where
      -- | (20-byte) SHA1 hash of info field.
      -- We're just hoping that no information is lost/chaged during
      -- bencode decoding -> encoding.
      infoHash :: B.ByteString
      infoHash = hashToBS . hash . LB.unpack . encode $ bv

      -- | Convert 20-byte hash to big-endian bytestring.
      hashToBS :: Word160 -> B.ByteString
      hashToBS (Word160 w1 w2 w3 w4 w5) =
        LB.toStrict . BB.toLazyByteString . mconcat . map BB.word32BE $ [w1, w2, w3, w4, w5]

      readPrivate :: Get Bool
      readPrivate = maybe False id <$> (optional $ field $ req "private")

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
