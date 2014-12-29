module Rho.PeerComms.PeerId where

import           Control.Applicative
import           Control.Monad
import qualified Data.ByteString         as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy    as LB
import           Data.Monoid
import           System.Random           (randomIO)

-- | 20-byte peer_id
newtype PeerId = PeerId { unwrapPeerId :: B.ByteString } deriving (Show, Ord, Eq)

-- | Generate 20-byte peer id.
generatePeerId :: IO PeerId
generatePeerId =
    PeerId . LB.toStrict . BB.toLazyByteString . mconcat . map BB.word8 <$> replicateM 20 randomIO
