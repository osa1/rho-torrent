{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Rho.PeerCommsSpec where

import           Control.Applicative
import           Control.Monad
import qualified Data.ByteString         as B
import qualified Data.Dequeue            as D
import           Data.IORef
import           Data.Maybe
import           Data.Monoid
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
    fromHUnitTest parseExtendedHsAndBF
    fromHUnitTest parsePieceReqs

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
            recvAndParse listener 26
            checkBuffer listener

parseExtendedHsAndBF :: Test
parseExtendedHsAndBF = TestLabel "parsing extended handshake followed by bitfield" $ TestCase $ do
  msg <- B.readFile (dataRoot </> "extended_hs_with_bf")
  emitter <- mkMessageEmitter msg
  listener <- initListener emitter
  recvAndParse listener 2
  checkBuffer listener

parsePieceReqs :: Test
parsePieceReqs = TestLabel "parsing piece requests" $ TestCase $ do
  msg <- B.readFile (dataRoot </> "piece_requests")
  emitter <- mkMessageEmitter msg
  listener <- initListener emitter
  recvAndParse listener 2
  checkBuffer listener

recvAndParse :: Listener -> Int -> Assertion
recvAndParse listener n = do
  msgs <- replicateM n (recvMessage listener)
  let ms = mapMaybe (\msg -> case msg of { Msg m -> Just m; _ -> Nothing }) msgs
  assertEqual "Failed to read some messages." n (length ms)
  let parsedMs = parseMsgs parsePeerMsg ms
  assertEqual "Failed to parse some messages." n (length parsedMs)

checkBuffer :: Listener -> Assertion
checkBuffer (Listener buf _ _ _ _) = do
    (d, l) <- readIORef buf
    unless (D.null d) $ do
      let bufContents = mconcat $ D.takeFront (D.length d) d
      assertFailure $ "Buffer is not empty: " ++ show bufContents
    assertEqual "Buffer length is not zero" 0 l

parseMsgs :: (B.ByteString -> Either err a) -> [B.ByteString] -> [a]
parseMsgs p ms = mapMaybe (\msg -> case p msg of { Left _ -> Nothing; Right m -> Just m}) ms

recvMsg :: RecvMsg -> Either B.ByteString B.ByteString
recvMsg (ConnClosed bs) = Left bs
recvMsg (Msg bs) = Right bs

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
