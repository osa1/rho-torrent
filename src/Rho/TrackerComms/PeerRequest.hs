{-# LANGUAGE ScopedTypeVariables #-}

module Rho.TrackerComms.PeerRequest where

import           Control.Applicative
import           Control.Exception
import qualified Data.ByteString.Char8         as BC
import           Data.Monoid
import           Network.Socket

import           Rho.SessionState
import           Rho.Tracker
import           Rho.TrackerComms.HTTP         (peerRequestHTTP)
import           Rho.TrackerComms.PeerResponse
import           Rho.TrackerComms.UDP          (initUDPCommHandler,
                                                peerRequestUDP)

requestPeers :: Session -> Tracker -> IO PeerResponse
requestPeers s@Session{sessPeerId=pid, sessInfoHash=hash, sessPort=port} (HTTPTracker uri) = do
    stats' <- stats s
    peerRequestHTTP pid port uri stats' hash >>= either error return
requestPeers s@Session{sessPeerId=pid, sessInfoHash=hash, sessPort=peerPort}
             (UDPTracker host port) = do
    addrInfo <-
      (Just <$> getAddrInfo (Just defaultHints) (Just $ BC.unpack host) (Just $ show port))
        `catch` \(_ :: IOException) -> return Nothing
    stats' <- stats s
    case addrInfo of
      Nothing -> return mempty
      Just addrInfo' -> do
        let trackerAddr = addrAddress (last addrInfo')
        commHandler <- initUDPCommHandler
        (peerRequestUDP commHandler trackerAddr pid hash stats' peerPort
            >>= either error return)
          `catch` \(_ :: IOException) -> return mempty
