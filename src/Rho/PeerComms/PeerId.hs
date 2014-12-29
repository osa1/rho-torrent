{-# LANGUAGE OverloadedStrings #-}

module Rho.PeerComms.PeerId where

import           Control.Monad
import qualified Data.ByteString as B
import           Data.Monoid
import           System.Random   (randomIO)

-- | 20-byte peer_id
newtype PeerId = PeerId { unwrapPeerId :: B.ByteString } deriving (Show, Ord, Eq)

-- | Generate 20-byte peer id.
generatePeerId :: IO PeerId
generatePeerId = (PeerId . ("-RH0100-" <>) . B.pack) `fmap` replicateM 12 randomIO
