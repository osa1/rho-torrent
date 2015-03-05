{-# LANGUAGE OverloadedStrings #-}

module Rho.TestTrackerSpec where

import           Network.Socket

import           Test.Hspec
import           Test.Hspec.Contrib.HUnit
import           Test.HUnit

import           Rho.ClientSpec                (mkPeerId)
import           Rho.Metainfo
import           Rho.MetainfoSpec              (parseMIAssertion)
import           Rho.Session
import           Rho.SessionState
import           Rho.TestTracker
import           Rho.TrackerComms.PeerRequest
import           Rho.TrackerComms.PeerResponse
import           Rho.TrackerComms.UDP

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "test tracker" $ do
      fromHUnitTest connTest

connTest :: Test
connTest = TestLabel "connect + request peers" $ TestCase $ do
    Metainfo{mInfo=info} <- parseMIAssertion "test/test.torrent"
    let pid = mkPeerId 1
    sess <- initTorrentSession info [] pid

    (_, SockAddrInet trackerPort _) <- runTracker

    udpHandler <- initUDPCommHandler
    pr <- requestPeersUDP sess udpHandler "127.0.0.1" trackerPort

    localhost <- inet_addr "127.0.0.1"
    assertEqual "wrong number of peers" [SockAddrInet (sessPort sess) localhost]
                                        (prPeers pr)
