module Rho.Torrent where

import           Data.Word

import           Rho.InfoHash
import           Rho.Magnet
import           Rho.Metainfo

data Torrent = Torrent
  { source     :: Either Metainfo Magnet
  , downloaded :: Word64
  , left       :: Word64
  , uploaded   :: Word64
  } deriving (Show)

mkTorrentFromMagnet :: Magnet -> Torrent
mkTorrentFromMagnet m = Torrent (Right m) 0 0 0

mkTorrentFromMetainfo :: Metainfo -> Torrent
mkTorrentFromMetainfo m = Torrent (Left m) 0 0 0

infoHash :: Torrent -> InfoHash
infoHash Torrent{source=Right Magnet{mHash=hash}} = hash
infoHash Torrent{source=Left Metainfo{mInfo=Info{iHash=hash}}} = hash
