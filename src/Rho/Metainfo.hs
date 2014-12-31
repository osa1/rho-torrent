{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, FlexibleInstances,
             OverloadedStrings, RecordWildCards #-}

module Rho.Metainfo where

import           Control.Applicative
import           Data.BEncode            as BE
import qualified Data.BEncode.BDict      as BE
import qualified Data.ByteString         as B
import qualified Data.ByteString.Lazy    as LB
import           Data.Digest.SHA1
import           Data.Maybe              (fromMaybe)
import           Data.Typeable
import           Data.Word
import           GHC.Generics

import           Rho.InfoHash
import           Rho.Tracker
import           Rho.Utils

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
  } deriving (Show, Eq, Typeable, Generic)

data Info = Info
  { iName        :: B.ByteString
  , iHash        :: InfoHash
  , iPieceLength :: Word32
  , iPieces      :: [B.ByteString]
  , iPrivate     :: Bool
  , iFiles       :: Either File [File]
    -- ^ Left: single-file mode, Right: multi-file mode
  } deriving (Show, Eq, Typeable, Generic)

torrentSize :: Info -> Word64
torrentSize Info{iFiles=Left (File len _ _)} = len
torrentSize Info{iFiles=Right files} = sum $ map fLength files

trackers :: Metainfo -> [Tracker]
trackers Metainfo{mAnnounce=t, mAnnounceList=ts} = (t:) . concat . fromMaybe [] $ ts

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

parseInfoDict :: B.ByteString -> Either String Info
parseInfoDict = decode

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

-- | (20-byte) SHA1 hash of info field.
mkInfoHash :: BValue -> InfoHash
mkInfoHash bv = InfoHash . word160ToBS . hash . LB.unpack . encode $ bv

instance BEncode Info where
  toBEncode (Info name _ pl ps priv (Right files)) = toDict $
       "files"        .=! files
    .: "name"         .=! name
    .: "piece length" .=! pl
    .: "pieces"       .=! B.concat ps
    .: "private"      .=? (if priv then Just True else Nothing)
    .: endDict

  toBEncode (Info name _ pl ps priv (Left (File flen md5 _))) = toDict $
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
        return $ Info name (mkInfoHash bv) pieceLength pieces private (Left $ File flen md5sum [])
    | otherwise = flip fromDict bv $ do
        -- multi file mode
        files <- field $ req "files"
        name <- field $ req "name"
        pieceLength <- field $ req "piece length"
        pieces <- splitPieces <$> field (req "pieces")
        private <- readPrivate
        return $ Info name (mkInfoHash bv) pieceLength pieces private (Right $ files)
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
