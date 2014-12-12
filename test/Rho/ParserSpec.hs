{-# LANGUAGE OverloadedStrings #-}

module Rho.ParserSpec where

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
