{-# LANGUAGE LambdaCase #-}

module Rho.TorrentLoop where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad
import           Data.Foldable                (for_)
import           Data.IORef
import           Data.List                    (sortBy, uncons)
import qualified Data.Map                     as M
import           System.Clock
import           System.Random                (randomRIO)

import           Rho.PeerComms.Message
import           Rho.PeerComms.PeerConnection
import           Rho.PeerComms.PeerConnState
import           Rho.PeerComms.PeerId
import           Rho.PieceMgr
import           Rho.SessionState
import           Rho.Utils

torrentLoop :: Session -> PieceMgr -> IO (Async ())
torrentLoop sess pMgr = async $ forever $ do
    peers <- M.elems <$> readMVar (sessPeers sess)

    forgetNonresponsiveInterestMsgs peers

    is <- numInterested peers
    when (is < 5) $ do
      missings <- missingPieces pMgr
      sendInteresteds peers missings (5 - is)

    -- We keep 4 interested peers at unchoked state
    luckyPeers <- numUnchokedAndInterested peers
    when (luckyPeers < 4) $
      sendUnchokes peers (4 - luckyPeers)

    maybeRotateOptimisticUnchoke sess

    -- We wait 10 seconds to prevent fibrillation.
    threadDelay (10 * 1000000)

maybeRotateOptimisticUnchoke :: Session -> IO ()
maybeRotateOptimisticUnchoke Session{sessPeers=peers, sessCurrentOptUnchoke=currentRef} =
    readIORef currentRef >>= \case
      Nothing ->
        -- We haven't unchoked any peers so far, go ahead.
        rotateOptimisticUnchoke Nothing
      Just current -> do
        -- Rotate only if we gave a chance to current peer for at least 30
        -- seconds.
        now <- getTime Monotonic
        currentPC <- readIORef current
        when (tsToSec (now `dt` pcLastUnchoke currentPC) >= 30) $
          rotateOptimisticUnchoke (Just $ pcPeerId currentPC)
  where
    rotateOptimisticUnchoke :: Maybe PeerId -> IO ()
    rotateOptimisticUnchoke pid = do
      peers' <- M.elems <$> readMVar peers
      peerConns <- zip peers' <$> mapM readIORef peers'
      let
        potentials = filter (peerFilter pid . snd) peerConns
        sorted =
          sortBy (\p1 p2 -> pcLastUnchoke (snd p1) `compare` pcLastUnchoke (snd p2)) potentials

      for_ (uncons sorted) $ \(h, t) -> do
        -- In case we have multiple peers with same "last unchoked" times, we
        -- pick someone random.
        let t' = takeWhile ((pcLastUnchoke (snd h) ==) . pcLastUnchoke . snd) t
        newPeer <- pickRandom (h : t')
        unchokePeer (fst newPeer)
        writeIORef currentRef $ Just (fst newPeer)

    -- | We choose a peer that is 1) different than currently unchoked one 2)
    -- currently choked 3) interested in something we have.
    peerFilter :: Maybe PeerId -> PeerConn -> Bool
    peerFilter Nothing    pc = pcPeerInterested pc && pcChoking pc
    peerFilter (Just pid) pc = pid /= pcPeerId pc && pcPeerInterested pc && pcChoking pc

-- TODO: Make sure these are optimized to non-allocating loops

numInterested :: [IORef PeerConn] -> IO Int
numInterested pcs = length <$> filterM (fmap pcInterested . readIORef) pcs

numPeerInterested :: [IORef PeerConn] -> IO Int
numPeerInterested pcs = length <$> filterM (fmap pcPeerInterested . readIORef) pcs

numUnchokedAndInterested :: [IORef PeerConn] -> IO Int
numUnchokedAndInterested pcs =
    length <$> filterM (fmap (\pc -> not (pcChoking pc) && pcPeerInterested pc) . readIORef) pcs

-- | Send NotInterested messages to peers that are not unchoked us since our
-- interested message in last turn.
forgetNonresponsiveInterestMsgs :: [IORef PeerConn] -> IO ()
forgetNonresponsiveInterestMsgs pcs =
    forM_ pcs $ \pc -> do
      pc' <- readIORef pc
      when (pcInterested pc' && pcPeerChoking pc') $ sendNotInterested pc

sendInteresteds :: [IORef PeerConn] -> [PieceIdx] -> Int -> IO ()
sendInteresteds pcs missings amt = return () -- TODO: implement this

sendNotInterested :: IORef PeerConn -> IO ()
sendNotInterested pc = do
    pc' <- atomicModifyIORef' pc $ \pc' ->
             let pc'' = pc'{pcInterested=False, pcRequest=Nothing} in (pc'', pc'')
    void $ sendMessage pc' NotInterested

-- FIXME: We should unchoke peers that we downloaded the most from
sendUnchokes :: [IORef PeerConn] -> Int -> IO ()
sendUnchokes pcs amt = do
    chokeds <- filterM (fmap pcChoking . readIORef) pcs
    idxs <- replicateM amt (randomRIO (0, length chokeds - 1))
    mapM_ (unchokePeer . (chokeds !!)) idxs

moveOptimisticUnchoke :: Maybe (IORef PeerConn) -> [IORef PeerConn] -> IO (IORef PeerConn)
moveOptimisticUnchoke currentOpt pcs = do
    notChoking <- filterM (fmap (not . pcChoking) . readIORef) pcs
    rand <- pickRandom notChoking
    maybe (return ()) sendChoke $ currentOpt
    unchokePeer rand
    return rand

sendChoke :: IORef PeerConn -> IO ()
sendChoke pc = do
    pc' <- atomicModifyIORef' pc $ \pc' -> let pc'' = pc'{pcChoking=True} in (pc'', pc'')
    void $ sendMessage pc' Choke

pickRandom :: [a] -> IO a
pickRandom []  = error "pickRandom: Empty list"
pickRandom lst = (lst !!) <$> randomRIO (0, length lst - 1)
