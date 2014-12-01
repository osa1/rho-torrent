{-# LANGUAGE OverloadedStrings #-}

module Rho.TrackerCommsSpec where

import           Control.Applicative
import           Control.Monad
import           Data.Bits
import qualified Data.ByteString               as B
import           Data.Word

import           Test.Hspec
import           Test.Hspec.HUnit
import           Test.Hspec.QuickCheck
import           Test.HUnit
import           Test.QuickCheck               hiding (Result)

import qualified Rho.Bitfield                  as BF
import           Rho.InfoHash
import           Rho.Parser
import           Rho.PeerComms.Handshake
import           Rho.TrackerComms.UDP.Request
import           Rho.TrackerComms.UDP.Response
import           Rho.TrackerComms.UDP.Types
import           Rho.Utils

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "communications with trackers" $ do
    fromHUnitTest connectRequest
    fromHUnitTest connectResponse

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
