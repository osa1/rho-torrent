-- | Handling requesting peers from trackers periodically.
--
-- FIXME: Find a way to test this code.
--
module Rho.TrackerComms.TrackerManager where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Data.List                     (foldl')
import qualified Data.Map                      as M
import qualified Data.Set                      as S
import           Data.Word
import           Network.Socket                (SockAddr)
import           System.Clock
import qualified System.Log.Logger             as L

import           Rho.SessionState
import           Rho.Tracker
import           Rho.TrackerComms.PeerRequest
import           Rho.TrackerComms.PeerResponse
import           Rho.Utils

data TrackerState = TrackerState
  { tsLastReq  :: TimeSpec
  , tsInterval :: Word32
  , tsJob      :: Maybe (Async PeerResponse)
  }

runTrackerManager :: Session -> IO (Async ())
runTrackerManager sess = async $ loop M.empty
  where
    loop :: M.Map Tracker TrackerState -> IO ()
    loop trackerStates = do
      trackers' <- readMVar (sessTrackers sess)
      newState <- loop' trackers' trackerStates
      threadDelay (60 * 1000000)
      loop newState

    loop' :: [Tracker] -> M.Map Tracker TrackerState -> IO (M.Map Tracker TrackerState)
    loop' [] trackerStates = return trackerStates
    loop' (tr : trs) trackerStates =
      case M.lookup tr trackerStates of
        Nothing -> do
          -- new tracker, initialize interval as 0 and send a request
          info "found new tracker, sending first request"
          now <- getTime Monotonic
          newJob <- async $ peerReq sess tr (sessNewPeers sess)
          loop' trs (M.insert tr (TrackerState now 0 (Just newJob)) trackerStates)
        Just (TrackerState lastReq int (Just job)) -> do
          resp <- poll job
          case resp of
            Nothing -> loop' trs trackerStates
            Just (Left err) -> do
              warning $ "error happened while requesting peers: "
                        ++ show err ++ ". sending request again."
              now <- getTime Monotonic
              newJob <- async $ peerReq sess tr (sessNewPeers sess)
              loop' trs (M.insert tr (TrackerState now int (Just newJob)) trackerStates)
            Just (Right (PeerResponse int' _ _ _)) -> do
              loop' trs (M.insert tr (TrackerState lastReq int' Nothing) trackerStates)
        Just (TrackerState lastReq int Nothing) -> do
          now <- getTime Monotonic
          if (fromIntegral (tsToSec (now `dt` lastReq)) >= int)
            then do
              job <- async $ peerReq sess tr (sessNewPeers sess)
              loop' trs (M.insert tr (TrackerState now int (Just job)) trackerStates)
            else
              loop' trs trackerStates

peerReq :: Session -> Tracker -> MVar (S.Set SockAddr) -> IO PeerResponse
peerReq sess tr@HTTPTracker{} newPeers = do
    timerThread <- async $ threadDelay (60 * 1000000) >> return Nothing
    resp <- async $ do
      resp@(PeerResponse _ _ _ newPeers') <- requestPeers sess tr
      info $ "got a peer response, adding " ++ show (length newPeers') ++ " peers."
      modifyMVar_ newPeers $ \ps -> return $ foldl' (flip S.insert) ps newPeers'
      return $ Just resp
    (_, ret) <- waitAnyCancel [timerThread, resp]
    maybe (peerReq sess tr newPeers) return ret
peerReq sess tr@UDPTracker{} newPeers = loop 0
  where
    loop :: Int -> IO PeerResponse
    loop i = do
      timerThread <- async $ threadDelay (15 * (2 ^ i)) >> return Nothing
      -- FIXME: this creates a socket in every call, and sockets are
      -- probably never closed
      resp <- async $ do
        resp@(PeerResponse _ _ _ newPeers') <- requestPeers sess tr
        info $ "got a peer response, adding " ++ show (length newPeers') ++ " peers."
        modifyMVar_ newPeers $ \ps -> return $ foldl' (flip S.insert) ps newPeers'
        return $ Just resp
      (_, ret) <- waitAnyCancel [timerThread, resp]
      maybe (loop (i+1)) return ret

logger :: String
logger = "Rho.TrackerComms.TrackerManager"

warning, info :: String -> IO ()
warning  = L.warningM logger
-- info = L.infoM logger
info = putStrLn
