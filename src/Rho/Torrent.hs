module Rho.Torrent where

import           Data.ByteString as B
import           Data.Word

import           Rho.Magnet
import           Rho.Metainfo

data Torrent = Torrent
  { source     :: Either Metainfo Magnet
  , downloaded :: Word64
  , left       :: Word64
  , uploaded   :: Word64
  } deriving (Show)

infoHash :: Torrent -> B.ByteString
infoHash Torrent{source=Right Magnet{mHash=hash}} = hash
infoHash Torrent{source=Left Metainfo{mInfo=Info{iHash=hash}}} = hash
