{-# LANGUAGE LambdaCase, ScopedTypeVariables, TupleSections #-}

module Rho.TorrentLoop where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad
import           Data.Foldable                (for_)
import           Data.List                    (sortOn, uncons)
import qualified Data.Map                     as M
import           Data.Maybe                   (catMaybes)
import           System.Clock
import           System.Random                (randomRIO)

import           Rho.PeerComms.Message
import           Rho.PeerComms.PeerConnection
import           Rho.PeerComms.PeerConnState
import           Rho.PeerComms.PeerId
import           Rho.PeerComms.PeerPieceAsgn
import           Rho.PieceMgr
import           Rho.SessionState
import           Rho.Utils

torrentLoop :: Session -> PieceMgr -> IO (Async ())
torrentLoop sess pMgr = async $ forever $ do
    peers :: [PeerConnRef'] <- readPeers sess

    forgetNonresponsiveInterestMsgs peers

    when (numInterested peers < 5) $ do
      ps <- piecesToReq sess
      let asgn = assignPieces' ps
      ps' <- catMaybes <$> mapM (\(pId, pIdx) -> fmap (, pIdx) <$> peerConnRefFromId sess pId) asgn
      sendInteresteds ps'

    -- We keep 4 interested peers at unchoked state
    let luckyPeers = numUnchokedAndInterested peers
    when (luckyPeers < 4) $ sendUnchokes peers (4 - luckyPeers)

    maybeRotateOptimisticUnchoke sess

    -- We wait 10 seconds to prevent fibrillation.
    threadDelay (10 * 1000000)

maybeRotateOptimisticUnchoke :: Session -> IO ()
maybeRotateOptimisticUnchoke sess@Session{sessPeers=peers} =
    currentOptUnchoke sess >>= \case
      Nothing ->
        -- We haven't unchoked any peers so far, go ahead.
        rotateOptimisticUnchoke Nothing
      Just current -> do
        -- Rotate only if we gave a chance to current peer for at least 30
        -- seconds.
        now <- getTime Monotonic
        currentPC <- fst <$> readPeerConnRef current
        when (tsToSec (now `dt` pcLastUnchoke currentPC) >= 30) $
          rotateOptimisticUnchoke (Just $ pcPeerId currentPC)
  where
    rotateOptimisticUnchoke :: Maybe PeerId -> IO ()
    rotateOptimisticUnchoke pid = do
      peerConns :: [PeerConnRef'] <-
        mapM readPeerConnRef =<< M.toList <$> readMVar peers

      current <- currentOptUnchoke sess >>= \case
                   Nothing -> return Nothing
                   Just r  -> Just . pcPeerId . fst <$> readPeerConnRef r
      let
        potentials = filter (peerFilter current . fst) peerConns
        sorted     = sortOn (pcLastUnchoke . fst) potentials

      for_ (uncons sorted) $ \(h, t) -> do
        -- In case we have multiple peers with same "last unchoked" times, we
        -- pick someone random.
        let t' = takeWhile ((pcLastUnchoke (fst h) ==) . pcLastUnchoke . fst) t
        newPeer <- pickRandom (h : t')
        unchokePeer (snd newPeer)
        setCurrentOptUnchoke sess (Just (pcPeerId (fst newPeer), snd newPeer))

    -- | We choose a peer that is 1) different than currently unchoked one 2)
    -- currently choked 3) interested in something we have.
    peerFilter :: Maybe PeerId -> PeerConn -> Bool
    peerFilter Nothing    pc = pcPeerInterested pc && pcChoking pc
    peerFilter (Just pid) pc = pid /= pcPeerId pc && pcPeerInterested pc && pcChoking pc

-- TODO: Make sure these are optimized to non-allocating loops

numInterested :: [PeerConnRef'] -> Int
numInterested = length . filter (pcInterested . fst)

numPeerInterested :: [PeerConnRef] -> IO Int
numPeerInterested = length <.> filterM (pcPeerInterested <.> peerConn)

numUnchokedAndInterested :: [PeerConnRef'] -> Int
numUnchokedAndInterested =
    length . filter (\(pc, _) -> not (pcChoking pc) && pcPeerInterested pc)

-- | Send NotInterested messages to peers that are not unchoked us since our
-- interested message in last turn.
forgetNonresponsiveInterestMsgs :: [PeerConnRef'] -> IO ()
forgetNonresponsiveInterestMsgs pcs =
    forM_ pcs $ \ref@(pc, _) ->
      when (pcInterested pc && pcPeerChoking pc) $ sendNotInterested ref

sendInteresteds :: [(PeerConnRef, PieceIdx)] -> IO ()
sendInteresteds pcs =
    forM_ pcs $ \(r, pIdx) -> do
      (_, ref) <- readPeerConnRef r
      sendInterested ref pIdx

sendNotInterested :: PeerConnRef' -> IO ()
sendNotInterested (pc, ref) = do
    pc' <- amIORef ref $ \pc' -> pc'{pcInterested=False, pcRequest=Nothing}
    void $ sendMessage pc' NotInterested

-- FIXME: We should unchoke peers that we downloaded the most from
sendUnchokes :: [PeerConnRef'] -> Int -> IO ()
sendUnchokes pcs amt =
    case filter (pcChoking . fst) pcs of
      []      -> return ()
      chokeds -> do
        idxs <- replicateM amt (randomRIO (0, length chokeds - 1))
        mapM_ (unchokePeer . snd . (chokeds !!)) idxs

pickRandom :: [a] -> IO a
pickRandom []  = error "pickRandom: Empty list"
pickRandom lst = (lst !!) <$> randomRIO (0, length lst - 1)
