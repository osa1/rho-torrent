-- | Handling requesting peers from trackers periodically.
module Rho.TrackerComms.TrackerManager where

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad
import qualified Data.Map                      as M
import qualified Data.Set                      as S
import           Data.Word
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

runTrackerManager :: Session -> MVar [Tracker] -> IO (Async ())
runTrackerManager sess trackers = async $ loop M.empty
  where
    loop :: M.Map Tracker TrackerState -> IO ()
    loop trackerStates = forever $ do
      trackers' <- readMVar trackers
      loop' trackers' trackerStates
      threadDelay (60 * 1000000)

    loop' :: [Tracker] -> M.Map Tracker TrackerState -> IO ()
    loop' [] _ = return ()
    loop' (tr : trs) trackerStates =
      case M.lookup tr trackerStates of
        Nothing -> do
          -- new tracker
          return ()
        Just (TrackerState lastReq int (Just job)) -> do
          resp <- poll job
          case resp of
            Nothing -> loop' trs trackerStates
            Just (Left err) -> do
              warning $ "error happened while requesting peers: " ++ show err
              now <- getTime Monotonic
              newJob <- async $ peerReq sess tr
              loop' trs (M.insert tr (TrackerState now int (Just newJob)) trackerStates)
            Just (Right (PeerResponse int' _ _ ps)) -> do
              modifyMVar_ (sessNewPeers sess) $ return . S.union (S.fromList ps)
              loop' trs (M.insert tr (TrackerState lastReq int' Nothing) trackerStates)
        Just (TrackerState lastReq int Nothing) -> do
          now <- getTime Monotonic
          if (fromIntegral (tsToSec (now `dt` lastReq)) >= int)
            then do
              job <- async $ peerReq sess tr
              loop' trs (M.insert tr (TrackerState now int (Just job)) trackerStates)
            else
              loop' trs trackerStates

peerReq :: Session -> Tracker -> IO PeerResponse
peerReq sess tr@HTTPTracker{} = do
    timerThread <- async $ threadDelay (60 * 1000000) >> return Nothing
    resp <- async $ Just <$> requestPeers sess tr
    (_, ret) <- waitAnyCancel [timerThread, resp]
    maybe (peerReq sess tr) return ret
peerReq sess tr@UDPTracker{} = loop 0
  where
    loop :: Int -> IO PeerResponse
    loop i = do
      timerThread <- async $ threadDelay (15 * (2 ^ i)) >> return Nothing
      -- FIXME: this creates a socket in every call, and sockets are
      -- probably never closed
      resp <- async $ Just <$> requestPeers sess tr
      (_, ret) <- waitAnyCancel [timerThread, resp]
      maybe (loop (i+1)) return ret

logger :: String
logger = "Rho.TrackerComms.TrackerManager"

warning :: String -> IO ()
warning  = L.warningM logger
