-- | This module implements a priority queue for all the pieces in a torrent.
-- We use piece indexes as keys, number of peers that have the piece as the
-- priorities, and peers that have the piece as keys.
--
-- TODO: Find a better name.
--
module Rho.PieceStats
  ( PieceStats
  , initPieceStats
  , addPiece
  , takeMins
  ) where

import qualified Data.IntPSQ          as PQ
import qualified Data.Set             as S

import           Rho.PeerComms.PeerId
import           Rho.PieceMgr         (PieceIdx)

newtype PieceStats = PieceStats (PQ.IntPSQ Int (S.Set PeerId))

initPieceStats :: PieceStats
initPieceStats = PieceStats PQ.empty

-- FIXME: This changes the priority if e.g. when we do
-- > addPiece p1 1
-- > addPiece p1 1 again.

addPiece :: PeerId -> PieceIdx -> PieceStats -> PieceStats
addPiece pid pIdx (PieceStats pq) =
    PieceStats . snd $ PQ.alter alter (fromIntegral pIdx) pq
  where
    alter :: Maybe (Int, S.Set PeerId) -> ((), Maybe (Int, S.Set PeerId))
    alter Nothing       = ((), Just (1, S.singleton pid))
    alter (Just (p, v)) = ((), Just (p + 1, S.insert pid v))

-- | Return at most N pieces with minimum availability. Returns less results
-- when there aren't enough pieces in the queue.
takeMins :: PieceStats -> Int -> [(PieceIdx, S.Set PeerId)]
takeMins _               0 = []
takeMins (PieceStats pq) n =
    maybe [] f (PQ.findMin pq)
  where
    f (pIdx, _, ps) =
      (fromIntegral pIdx, ps) : takeMins (PieceStats $ PQ.deleteMin pq) (n - 1)
