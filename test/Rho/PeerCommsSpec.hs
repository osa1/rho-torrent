{-# LANGUAGE OverloadedStrings #-}

module Rho.PeerCommsSpec where

import           Control.Monad
import qualified Data.ByteString           as B

import           Test.Hspec
import           Test.Hspec.HUnit
import           Test.Hspec.QuickCheck
import           Test.HUnit
import           Test.QuickCheck           hiding (Result)
import           Test.QuickCheck.Instances ()

import           Rho.PeerComms.Handshake

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "handshake" $ do
    modifyMaxSuccess (const 10000) $ prop "printing-parsing" $ \(InfoHash infoHash, PeerId peerId) ->
      parseHandshake (mkHandshake infoHash peerId) == Right (infoHash, peerId, "")

newtype InfoHash = InfoHash B.ByteString deriving (Show)
newtype PeerId   = PeerId B.ByteString   deriving (Show)

gen20Bytes :: Gen B.ByteString
gen20Bytes = B.pack `fmap` replicateM 20 arbitrary

instance Arbitrary InfoHash where
    arbitrary = InfoHash `fmap` gen20Bytes
    shrink _  = []

instance Arbitrary PeerId where
    arbitrary = PeerId `fmap` gen20Bytes
    shrink _  = []
