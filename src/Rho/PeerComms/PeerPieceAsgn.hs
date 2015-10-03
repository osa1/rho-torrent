-- | Strategies for assigning pieces to peers.
-- (to be used in sending piece requests)
module Rho.PeerComms.PeerPieceAsgn where

import qualified Data.Map                    as M
import           Data.Maybe
import qualified Data.Set                    as S

import           Rho.PeerComms.Message       (PieceIdx)
import           Rho.PeerComms.PeerConnState

-- | Generate assignments of pieces to peers. Resulting list will have
-- these properties:
-- - Every peer will have at most one assignment.
-- - Every piece will be assigned at most once.
-- - If a piece has exactly one provider, that piece will be assigned to
--   that peer.
assignPieces
  :: [PieceIdx] -- ^ missing piece indexes
  -> M.Map PeerConn (S.Set PieceIdx) -- ^ (peer -> available pieces) map
  -> [(PeerConn, PieceIdx)] -- ^ assignments of pieces to peers
assignPieces missings peers = assignPieces' (piecePeers missings peers)

assignPieces' :: [(PieceIdx, S.Set PeerConn)] -> [(PeerConn, PieceIdx)]
assignPieces' pps =
  let
    -- we first assign a piece to a peer when the peer is only provider
    -- of the piece
    g = span ((==) 1 . S.size . snd) . filter (not . S.null . snd) $ pps
  in
    case g of
      ([], []) -> [] -- end of the algortihm

      (((pd, pcs) : _), others) ->
        let
          -- generate assignments
          asgn :: (PeerConn, PieceIdx)
          asgn = (S.elemAt 0 pcs, pd)

          -- remove assigned peer from sets of providers
          updatedPps :: [(PieceIdx, S.Set PeerConn)]
          updatedPps =
            filter (not . S.null . snd) $
              map (fmap (S.delete $ fst asgn)) others

          -- we loop on updated provider list
          rest = assignPieces' updatedPps
        in
          asgn : rest

      ([], ((pd, pcs) : ps)) ->
        -- just make a random assignment and iterate
        -- we know pcs can't be empty
        let
          usedPeer = S.elemAt 0 pcs
          asgn     = (usedPeer, pd)

          -- (no need to filter empties here, because we know all sets
          -- had at least 2 elements)
          updatedPps :: [(PieceIdx, S.Set PeerConn)]
          updatedPps = map (fmap (S.delete usedPeer)) ps

          rest = assignPieces' updatedPps
        in
          asgn : rest

piecePeers :: [PieceIdx] -> M.Map PeerConn (S.Set PieceIdx) -> [(PieceIdx, S.Set PeerConn)]
piecePeers missings peers =
    filter (not . S.null . snd) $
      flip map missings $ \missing -> (missing, collectPiecePeers missing (M.toList peers))

collectPiecePeers :: PieceIdx -> [(PeerConn, S.Set PieceIdx)] -> S.Set PeerConn
collectPiecePeers pIdx peers = S.fromList ps
  where
    ps = flip mapMaybe peers $ \(peer, peerPieces) ->
           if S.member pIdx peerPieces
             then Just peer
             else Nothing
