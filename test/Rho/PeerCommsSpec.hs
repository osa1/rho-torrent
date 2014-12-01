{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Rho.PeerCommsSpec where

import           Control.Applicative
import           Control.Monad
import qualified Data.ByteString         as B
import           Data.IORef
import           Data.Maybe
import           System.FilePath

import           Test.Hspec
import           Test.Hspec.HUnit
import           Test.Hspec.QuickCheck
import           Test.HUnit
import           Test.QuickCheck         hiding (Result)

import qualified Rho.Bitfield            as BF
import           Rho.InfoHash
import           Rho.Listener
import           Rho.PeerComms
import           Rho.PeerComms.Handshake
import           Rho.PeerComms.Message

dataRoot :: FilePath
dataRoot = "test/test_data/"

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "messaging with peers" $ do
    -- no need to test a lot of times since only thing that'll chance is
    -- info_hash and peer_id
    modifyMaxSuccess (const 100) $ prop "printing-parsing handshake" $ \(infoHash, peerId) ->
      parseHandshake (mkHandshake infoHash peerId) == Right (Handshake infoHash peerId Supports)

    modifyMaxSuccess (const 10000) $ prop "printing-parsing messages" $ \msg ->
      (mkPeerMsg defaultMsgTable msg >>= parsePeerMsg) == Right msg

    fromHUnitTest parseLongMsg

parseLongMsg :: Test
parseLongMsg = TestLabel "parsing long message (using listener, starting with handshake)" $
  TestCase $ do
    extendedMsg <- B.readFile (dataRoot </> "extended_msg")
    emitter <- mkMessageEmitter extendedMsg
    listener <- initListener emitter
    hsMsg <- recvHandshake listener
    case hsMsg of
      ConnClosed _ -> assertFailure "Receiving handshake failed"
      Msg hs ->
        case parseHandshake hs of
          Left err -> assertFailure $ "Parsing handshake failed: " ++ err
          Right _ -> do
            msgs <- replicateM 26 (recvMessage listener)
            let ms = mapMaybe (\msg -> case msg of { Msg m -> Just m; _ -> Nothing }) msgs
            assertEqual "Failed to read some messages." 26 (length ms)
            let parsedMs = flip mapMaybe ms $ \m -> case parsePeerMsg m of
                                                      Left _ -> Nothing
                                                      Right m' -> Just m'
            assertEqual "Failed to parse some messages." 26 (length parsedMs)

-- | Sends messages one byte at a time.
mkMessageEmitter :: B.ByteString -> IO (IO B.ByteString)
mkMessageEmitter msg = do
    msgRef <- newIORef msg
    return $ do
      msg <- readIORef msgRef
      case B.uncons msg of
        Just (w, rest) -> do
          writeIORef msgRef rest
          return (B.singleton w)
        Nothing -> return B.empty -- signal closed socket


-- * Arbitrary stuff

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
      , Extended <$> arbitrary
      ]

    shrink _ = [] -- TODO: maybe implement this

instance Arbitrary ExtendedPeerMsg where
    arbitrary = oneof
      [ ExtendedHandshake defaultMsgTable . (:[]) . UtMetadataSize <$> arbitrary
      , return $ ExtendedHandshake defaultMsgTable []
      , MetadataRequest <$> arbitrary
      , MetadataData <$> arbitrary <*> arbitrary <*> genBytes 10
      , MetadataReject <$> arbitrary
      ]

    shrink _ = undefined
