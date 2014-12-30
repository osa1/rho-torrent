{-# LANGUAGE OverloadedStrings #-}

module Rho.TrackerCommsSpec where

import qualified Data.ByteString               as B
import           Network.URI

import           Test.Hspec
import           Test.Hspec.Contrib.HUnit
import           Test.HUnit

import           Rho.InfoHash
import           Rho.PeerComms.PeerId
import           Rho.TrackerComms.HTTP
import           Rho.TrackerComms.UDP.Request
import           Rho.TrackerComms.UDP.Response
import           Rho.Utils

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "communications with trackers" $ do
    fromHUnitTest connectRequest
    fromHUnitTest connectResponse
    fromHUnitTest httpPeerRequest

connectRequest :: Test
connectRequest = TestLabel "connect request" $ TestCase $ do
  let connectReq = [ 0x00, 0x00, 0x04, 0x17, 0x27,
                     0x10, 0x19, 0x80, 0x00, 0x00,
                     0x00, 0x00, 0xf3, 0xe2, 0x88, 0xd2 ]
  assertEqual "wrong connect req message encoding"
              (B.pack connectReq)
              (mkTrackerMsg $ ConnectRequest (mkWord32 0xf3 0xe2 0x88 0xd2))

connectResponse :: Test
connectResponse = TestLabel "connect response response" $ TestCase $ do
  let connectResp = [ 0x00, 0x00, 0x00, 0x00, 0xa3,
                      0x41, 0x60, 0x42, 0xa3, 0xfa,
                      0x13, 0x43, 0xd8, 0x06, 0x97, 0xc5 ]
  assertEqual "wrong connect response decoding"
              (Right $ ConnectResponse (mkWord32 0xa3 0x41 0x60 0x42)
                                       (mkWord64 0xa3 0xfa 0x13 0x43 0xd8 0x06 0x97 0xc5))
              (parseUDPResponse $ B.pack connectResp)

httpPeerRequest :: Test
httpPeerRequest = TestLabel "HTTP peer request" $ TestCase $ do
  pid <- generatePeerId
  let infoHash = InfoHash $ B.pack
        [0x08, 0x89, 0xcf, 0x68, 0xcf, 0x4a, 0x7a, 0xb7, 0xf1, 0xdb,
         0x69, 0xc2, 0xff, 0xab, 0xe3, 0xdb, 0xfe, 0x53, 0xd0, 0x95]
  case parseURI "http://tracker.archlinux.org:6969/announce" of
    Nothing  -> assertFailure "failed to parse URI"
    Just uri -> do
      ret <- peerRequestHTTP pid (fromIntegral (1234 :: Int)) uri (0, 0, 0) infoHash
      case ret of
        Left err -> assertFailure $ "peer request failed: " ++ err
        Right _  -> return ()
