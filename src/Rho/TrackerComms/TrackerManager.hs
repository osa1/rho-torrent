-- | Handling requesting peers from trackers periodically.
--
-- FIXME: Find a way to test this code.
--
module Rho.TrackerComms.TrackerManager where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad
import qualified Data.Map                      as M
import           Data.Word
import           Network.Socket                (SockAddr)
import           System.Clock
import qualified System.Log.Logger             as L
import           System.Timeout

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

runTrackerManager :: Session -> IO (Chan SockAddr, Async ())
runTrackerManager sess = do
    -- TODO: maybe lazily initialize UDP comm handler, no need to
    -- initialize it when we don't have and UDP trackers.
    udpHandler <- initUDPCommHandler
    chan <- newChan
    thread <- async $ loop udpHandler M.empty chan
    return (chan, thread)
  where
    loop :: UDPCommHandler -> M.Map Tracker TrackerState -> Chan SockAddr -> IO ()
    loop udpHandler trackerStates chan = do
      trackers' <- readMVar (sessTrackers sess)
      newState <- loop' udpHandler trackers' trackerStates chan
      threadDelay (60 * 1000000)
      loop udpHandler newState chan

    loop'
      :: UDPCommHandler
      -> [Tracker] -> M.Map Tracker TrackerState
      -> Chan SockAddr -> IO (M.Map Tracker TrackerState)
    loop' _ [] trackerStates _ = return trackerStates
    loop' udpHandler (tr : trs) trackerStates chan =
      case M.lookup tr trackerStates of
        Nothing -> do
          -- new tracker, initialize interval as 0 and send a request
          info "found new tracker, sending first request"
          now <- getTime Monotonic
          newJob <- async $ peerReq sess udpHandler tr chan
          loop' udpHandler trs (M.insert tr (TrackerState now 0 (Just newJob)) trackerStates) chan
        Just (TrackerState lastReq int (Just job)) -> do
          resp <- poll job
          case resp of
            Nothing -> loop' udpHandler trs trackerStates chan
            Just (Left err) -> do
              warning $ "error happened while requesting peers: "
                        ++ show err ++ ". sending request again."
              now <- getTime Monotonic
              newJob <- async $ peerReq sess udpHandler tr chan
              loop' udpHandler trs (M.insert tr (TrackerState now int (Just newJob)) trackerStates) chan
            Just (Right (PeerResponse int' _ _ _)) -> do
              loop' udpHandler trs (M.insert tr (TrackerState lastReq int' Nothing) trackerStates) chan
        Just (TrackerState lastReq int Nothing) -> do
          now <- getTime Monotonic
          if (fromIntegral (tsToSec (now `dt` lastReq)) >= int)
            then do
              job <- async $ peerReq sess udpHandler tr chan
              loop' udpHandler trs (M.insert tr (TrackerState now int (Just job)) trackerStates) chan
            else
              loop' udpHandler trs trackerStates chan

peerReq :: Session -> UDPCommHandler -> Tracker -> Chan SockAddr -> IO PeerResponse
peerReq sess udpHandler tr@(HTTPTracker uri) chan = do
    ret <- timeout (60 * 1000000) $ requestPeersHTTP sess uri
    case ret of
      Nothing -> peerReq sess udpHandler tr chan
      Just resp@(PeerResponse _ _ _ newPeers) -> do
        info $ "got a peer response, adding " ++ show (length newPeers) ++ " peers."
        forM_ newPeers $ writeChan chan
        return resp
peerReq sess udpHandler (UDPTracker host port) chan = loop 0
  where
    loop :: Int -> IO PeerResponse
    loop i = do
      ret <- timeout (15 * (2 ^ i)) $ requestPeersUDP sess udpHandler host port
      case ret of
        Nothing -> loop (i + 1)
        Just resp@(PeerResponse _ _ _ newPeers) -> do
          info $ "got a peer response, adding " ++ show (length newPeers) ++ " peers."
          forM_ newPeers $ writeChan chan
          return resp

logger :: String
logger = "Rho.TrackerComms.TrackerManager"

warning, info :: String -> IO ()
warning  = L.warningM logger
info = L.infoM logger
