module Rho.TrackerComms.PeerResponse where

import           Data.Word      (Word32)
import           Network.Socket (SockAddr)

data PeerResponse = PeerResponse
  { prInterval :: Word32
  , prLeechers :: Maybe Word32
  , prSeeders  :: Maybe Word32
  , prPeers    :: [SockAddr]
  } deriving (Show)
