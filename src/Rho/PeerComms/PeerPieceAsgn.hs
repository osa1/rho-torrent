-- | Strategies for assigning pieces to peers.
-- (to be used in sending piece requests)
module Rho.PeerComms.PeerPieceAsgn where

import qualified Data.Map                    as M
import           Data.Maybe
import qualified Data.Set                    as S
import           Data.Word

import           Rho.PeerComms.PeerConnState
import           Rho.PieceMgr

-- | Generate assignments of pieces to peers. Resulting list will have
-- these properties:
-- - Every peer will have at most one assignment.
-- - Every piece will be assigned at most once.
-- - If a piece has exactly one provider, that piece will be assigned to
--   that peer.
assignPieces
  :: [PieceData] -- ^ missing (pieceIdx, pieceOffset, pieceLength) triples
  -> M.Map PeerConn (S.Set Word32) -- ^ (peer -> available pieces) map
  -> [(PeerConn, PieceData)] -- ^ assignments of pieces to peers
assignPieces missings peers = loop piecePeers
  where
    loop :: [(PieceData, S.Set PeerConn)] -> [(PeerConn, PieceData)]
    loop pps =
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
              asgn :: (PeerConn, PieceData)
              asgn = (S.elemAt 0 pcs, pd)

              -- remove assigned peer from sets of providers
              updatedPps :: [(PieceData, S.Set PeerConn)]
              updatedPps =
                filter (not . S.null . snd) $
                  map (fmap (S.delete $ fst asgn)) others

              -- we loop on updated provider list
              rest = loop updatedPps
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
              updatedPps :: [(PieceData, S.Set PeerConn)]
              updatedPps = map (fmap (S.delete usedPeer)) ps

              rest = loop updatedPps
            in
              asgn : rest

    piecePeers :: [(PieceData, S.Set PeerConn)]
    piecePeers =
      filter (not . S.null . snd) $
        flip map missings $ \missing -> (missing, collectPiecePeers missing)

    collectPiecePeers :: PieceData -> S.Set PeerConn
    collectPiecePeers (pIdx, _, _) =
      let
        ps = flip mapMaybe (M.toList peers) $ \(peer, peerPieces) ->
               if S.member pIdx peerPieces
                 then Just peer
                 else Nothing
      in
        S.fromList ps
