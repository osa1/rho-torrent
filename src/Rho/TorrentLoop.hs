module Rho.TorrentLoop where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad
import           Data.IORef
import qualified Data.Map                    as M
import           System.Clock

import           Rho.PeerComms.PeerConnState
import           Rho.SessionState
import           Rho.Utils

torrentLoop :: Session -> TimeSpec -> IO (Async ())
torrentLoop sess lastTurn = async $ forever $ do
    now <- getTime Monotonic
    peers <- M.elems <$> readMVar (sessPeers sess)

    is <- numInterested peers

    -- We keep 4 interested peers at unchoked state
    luckyPeers <- numUnchokedAndInterested peers

    forgetNonresponsiveInterestMsgs peers

    when (is < 5) $
      sendInteresteds peers (5 - is)

    when (luckyPeers < 4) $
      sendUnchokes peers (4 - luckyPeers)

    when (tsToSec (now `dt` lastTurn) >= 30) $
      moveOptimisticUnchoke peers

    -- We wait 10 seconds to prevent fibrillation.
    threadDelay (10 * 1000000)

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

sendInteresteds :: [IORef PeerConn] -> Int -> IO ()
sendInteresteds _ _ = return () -- TODO: Implement this

sendNotInterested :: IORef PeerConn -> IO ()
sendNotInterested _ = return ()

-- NOTE: We should unchoke peers that we downloaded the most from
sendUnchokes :: [IORef PeerConn] -> Int -> IO ()
sendUnchokes _ _ = return () -- TODO: implement this

moveOptimisticUnchoke :: [IORef PeerConn] -> IO ()
moveOptimisticUnchoke _ = return () -- TODO: implement this
