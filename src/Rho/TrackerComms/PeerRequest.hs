{-# LANGUAGE ScopedTypeVariables #-}

module Rho.TrackerComms.PeerRequest where

import           Control.Exception
import qualified Data.ByteString.Char8         as BC
import           Network.Socket
import           Network.URI                   (URI)

import           Rho.SessionState
import           Rho.TrackerComms.HTTP         (peerRequestHTTP)
import           Rho.TrackerComms.PeerResponse
import           Rho.TrackerComms.UDP          (UDPCommHandler, peerRequestUDP)

requestPeersHTTP :: Session -> URI -> IO PeerResponse
requestPeersHTTP s@Session{sessPeerId=pid, sessInfoHash=hash, sessPort=port} uri = do
    stats' <- stats s
    peerRequestHTTP pid port uri stats' hash >>= either error return

requestPeersUDP
  :: Session -> UDPCommHandler -> BC.ByteString -> PortNumber -> IO PeerResponse
requestPeersUDP s@Session{sessPeerId=pid, sessInfoHash=hash, sessPort=peerPort}
                commHandler host port = do
    addrInfo <-
      (Just <$> getAddrInfo (Just defaultHints) (Just $ BC.unpack host) (Just $ show port))
        `catch` \(_ :: IOException) -> return Nothing
    case addrInfo of
      Nothing -> return mempty
      Just addrInfo' -> do
        let trackerAddr = addrAddress (last addrInfo')
        stats' <- stats s
        (peerRequestUDP commHandler trackerAddr pid hash stats' peerPort
            >>= either error return)
          `catch` \(_ :: IOException) -> return mempty
