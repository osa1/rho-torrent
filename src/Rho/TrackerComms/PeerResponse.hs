module Rho.TrackerComms.PeerResponse where

import           Data.Monoid
import           Data.Word      (Word32)
import           Network.Socket (SockAddr)

data PeerResponse = PeerResponse
  { prInterval :: Word32
  , prLeechers :: Maybe Word32
  , prSeeders  :: Maybe Word32
  , prPeers    :: [SockAddr]
  } deriving (Show, Eq)

instance Monoid PeerResponse where
    mempty = PeerResponse 0 Nothing Nothing []
    PeerResponse i1 ls1 ss1 ps1 `mappend` PeerResponse i2 ls2 ss2 ps2 =
        PeerResponse (max i1 i2) (ls1 `mw` ls2) (ss1 `mw` ss2) (ps1 <> ps2)
      where
        Nothing `mw` Nothing = Nothing
        Just w  `mw` Nothing = Just w
        Nothing `mw` Just w  = Just w
        Just w1 `mw` Just w2 = Just (w1 + w2)
