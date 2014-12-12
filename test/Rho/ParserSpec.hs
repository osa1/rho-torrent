{-# LANGUAGE OverloadedStrings #-}

module Rho.ParserSpec where

import           Control.Monad
import qualified Data.ByteString.Char8 as BC

import           Test.Hspec
import           Test.Hspec.HUnit
import           Test.HUnit

import           Rho.Tracker

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "tests for various parsers" $ do
    fromHUnitTest $ TestLabel "parseTrackerBS error reporting - missing colon in UDP addr" $
      TestCase $ do
        let str = "udp://127.0.0.1"
        assertEqual "wrong error message" (Left $ "Can't parse port number in " ++ drop 6 str)
                                          (parseTrackerBS $ BC.pack str)

    fromHUnitTest $ TestLabel "parseTrackerBS error reporting - missing port after colon" $
      TestCase $ do
        let str = "udp://127.0.0.1:"
        assertEqual "wrong error message" (Left "Can't parse port number from ")
                                          (parseTrackerBS $ BC.pack str)

    fromHUnitTest $ TestLabel "parseTrackerBS error reporting - deciding UDP/HTTP" $ TestCase $ do
      let str = "localhost:80/announce"
      assertEqual "wrong error message"
        (Left $ "Can't decide whether the host is UDP or HTTP: " ++ str)
        (parseTrackerBS $ BC.pack str)

    fromHUnitTest $ TestLabel "parsing some tracker addresses seen in the wild" $ TestCase $ do
      let trackers =
            [ "udp://tracker.publicbt.com:80/announce"
            , "udp://tracker.openbittorrent.com:80/announce"
            , "udp://fr33domtracker.h33t.com:3310/announce"
            , "udp://tracker.istole.it:80/announce"
            , "udp://tracker.seedceo.com:2710/announce"
            , "http://fr33dom.h33t.com:3310/announce"
            , "udp://coppersurfer.tk:6969/announce"
            , "udp://tracker.openbittorrent.com:80/"
            , "udp://tracker.publicbt.com:80"
            , "udp://tracker.ccc.de:80"
            , "udp://tracker.istole.it:80"
            ]
      forM_ trackers $ \tracker ->
        case parseTrackerBS tracker of
          Left err -> assertFailure $ "Failed to parse \"" ++ BC.unpack tracker ++ "\": " ++ err
          Right _  -> return ()
