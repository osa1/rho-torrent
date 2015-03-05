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
import           Rho.TrackerComms.UDP
import           Rho.Utils

data TrackerState = TrackerState
  { tsLastReq  :: TimeSpec
  , tsInterval :: Word32
  , tsJob      :: Maybe (Async PeerResponse)
  }

runTrackerManager :: Session -> IO (Async ())
runTrackerManager sess = do
    -- TODO: mayby lazily initialize UDP comm handler, no need to
    -- initialize it when we don't have and UDP trackers.
    udpHandler <- initUDPCommHandler
    async $ loop udpHandler M.empty
  where
    loop :: UDPCommHandler -> M.Map Tracker TrackerState -> IO ()
    loop udpHandler trackerStates = do
      trackers' <- readMVar (sessTrackers sess)
      newState <- loop' udpHandler trackers' trackerStates
      threadDelay (60 * 1000000)
      loop udpHandler newState

    loop'
      :: UDPCommHandler
      -> [Tracker] -> M.Map Tracker TrackerState -> IO (M.Map Tracker TrackerState)
    loop' _ [] trackerStates = return trackerStates
    loop' udpHandler (tr : trs) trackerStates =
      case M.lookup tr trackerStates of
        Nothing -> do
          -- new tracker, initialize interval as 0 and send a request
          info "found new tracker, sending first request"
          now <- getTime Monotonic
          newJob <- async $ peerReq sess udpHandler tr (sessNewPeers sess)
          loop' udpHandler trs (M.insert tr (TrackerState now 0 (Just newJob)) trackerStates)
        Just (TrackerState lastReq int (Just job)) -> do
          resp <- poll job
          case resp of
            Nothing -> loop' udpHandler trs trackerStates
            Just (Left err) -> do
              warning $ "error happened while requesting peers: "
                        ++ show err ++ ". sending request again."
              now <- getTime Monotonic
              newJob <- async $ peerReq sess udpHandler tr (sessNewPeers sess)
              loop' udpHandler trs (M.insert tr (TrackerState now int (Just newJob)) trackerStates)
            Just (Right (PeerResponse int' _ _ _)) -> do
              loop' udpHandler trs (M.insert tr (TrackerState lastReq int' Nothing) trackerStates)
        Just (TrackerState lastReq int Nothing) -> do
          now <- getTime Monotonic
          if (fromIntegral (tsToSec (now `dt` lastReq)) >= int)
            then do
              job <- async $ peerReq sess udpHandler tr (sessNewPeers sess)
              loop' udpHandler trs (M.insert tr (TrackerState now int (Just job)) trackerStates)
            else
              loop' udpHandler trs trackerStates

peerReq :: Session -> UDPCommHandler -> Tracker -> MVar (S.Set SockAddr) -> IO PeerResponse
peerReq sess udpHandler tr@(HTTPTracker uri) newPeers = do
    timerThread <- async $ threadDelay (60 * 1000000) >> return Nothing
    resp <- async $ do
      resp@(PeerResponse _ _ _ newPeers') <- requestPeersHTTP sess uri
      info $ "got a peer response, adding " ++ show (length newPeers') ++ " peers."
      modifyMVar_ newPeers $ \ps -> return $ foldl' (flip S.insert) ps newPeers'
      return $ Just resp
    (_, ret) <- waitAnyCancel [timerThread, resp]
    maybe (peerReq sess udpHandler tr newPeers) return ret
peerReq sess udpHandler (UDPTracker host port) newPeers = loop 0
  where
    loop :: Int -> IO PeerResponse
    loop i = do
      timerThread <- async $ threadDelay (15 * (2 ^ i)) >> return Nothing
      -- FIXME: this creates a socket in every call, and sockets are
      -- probably never closed
      resp <- async $ do
        resp@(PeerResponse _ _ _ newPeers') <- requestPeersUDP sess udpHandler host port
        info $ "got a peer response, adding " ++ show (length newPeers') ++ " peers."
        modifyMVar_ newPeers $ \ps -> return $ foldl' (flip S.insert) ps newPeers'
        return $ Just resp
      (_, ret) <- waitAnyCancel [timerThread, resp]
      maybe (loop (i+1)) return ret

logger :: String
logger = "Rho.TrackerComms.TrackerManager"

warning, info :: String -> IO ()
warning  = L.warningM logger
info = L.infoM logger
