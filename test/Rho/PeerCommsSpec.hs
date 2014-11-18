{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Rho.PeerCommsSpec where

import           Control.Applicative
import           Control.Monad
import qualified Data.ByteString         as B
import qualified Data.Map                as M

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck         hiding (Result)

import qualified Rho.Bitfield            as BF
import           Rho.InfoHash
import           Rho.PeerComms.Handshake
import           Rho.PeerComms.Message

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "messaging with peers" $ do
    -- no need to test a lot of times since only thing that'll chance is
    -- info_hash and peer_id
    modifyMaxSuccess (const 100) $ prop "printing-parsing handshake" $ \(infoHash, peerId) ->
      parseHandshake (mkHandshake infoHash peerId) == Right (infoHash, peerId, "")

    modifyMaxSuccess (const 100000) $ prop "printing-parsing messages (not extended)" $ \msg ->
      parsePeerMsg M.empty (mkPeerMsg msg) == Just msg

genBytes :: Int -> Gen B.ByteString
genBytes n = B.pack `fmap` replicateM n arbitrary

instance Arbitrary InfoHash where
    arbitrary = InfoHash `fmap` genBytes 20
    shrink _  = []

instance Arbitrary PeerId where
    arbitrary = PeerId `fmap` genBytes 20
    shrink _  = []

instance Arbitrary BF.Bitfield where
    -- let's generate fixed length for now
    arbitrary = BF.Bitfield <$> genBytes 5
    shrink _ = []

instance Arbitrary PeerMsg where
    arbitrary = oneof
      [ return KeepAlive
      , return Choke
      , return Interested
      , return NotInterested
      , Have <$> arbitrary
      , Bitfield <$> arbitrary
      , Request <$> arbitrary <*> arbitrary <*> arbitrary
      , Piece <$> arbitrary <*> arbitrary <*> genBytes 20
      , Cancel <$> arbitrary <*> arbitrary <*> arbitrary
      -- TODO: fix this
      -- , Port . fromIntegral <$> (arbitrary :: Gen Word16)
      -- TODO: add Extended
      ]

    shrink _ = [] -- TODO: maybe implement this
