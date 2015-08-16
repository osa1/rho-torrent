module Rho.TorrentLoop where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad
import           Data.IORef
import qualified Data.Map                     as M
import           System.Clock
import           System.Random                (randomRIO)

import           Rho.PeerComms.Message
import           Rho.PeerComms.PeerConnection
import           Rho.PeerComms.PeerConnState
import           Rho.PieceMgr
import           Rho.SessionState
import           Rho.Utils

torrentLoop :: Session -> PieceMgr -> TimeSpec -> IO (Async ())
torrentLoop sess pMgr lastTurn = async $ forever $ do
    now <- getTime Monotonic
    peers <- M.elems <$> readMVar (sessPeers sess)

    is <- numInterested peers

    -- We keep 4 interested peers at unchoked state
    luckyPeers <- numUnchokedAndInterested peers

    forgetNonresponsiveInterestMsgs peers

    when (is < 5) $ do
      missings <- missingPieces pMgr
      sendInteresteds peers missings (5 - is)

    when (luckyPeers < 4) $
      sendUnchokes peers (4 - luckyPeers)

    when (tsToSec (now `dt` lastTurn) >= 30) $ do
      currentOpt <- readIORef (sessCurrentOptUnchoke sess)
      newOpt <- moveOptimisticUnchoke currentOpt peers
      writeIORef (sessCurrentOptUnchoke sess) (Just newOpt)

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

sendInteresteds :: [IORef PeerConn] -> [PieceIdx] -> Int -> IO ()
sendInteresteds pcs missings amt = return () -- TODO: implement this

sendNotInterested :: IORef PeerConn -> IO ()
sendNotInterested pc = do
    pc' <- atomicModifyIORef' pc $ \pc' -> let pc'' = pc'{pcInterested=False} in (pc'', pc'')
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
