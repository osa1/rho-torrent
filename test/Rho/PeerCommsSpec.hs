{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Rho.PeerCommsSpec where

import           Control.Applicative
import           Control.Concurrent.Async
import           Control.Monad
import qualified Data.ByteString              as B
import qualified Data.ByteString.Builder      as BB
import qualified Data.ByteString.Lazy         as LB
import qualified Data.Dequeue                 as D
import           Data.IORef
import           Data.List
import qualified Data.Map                     as M
import           Data.Maybe
import           Data.Monoid
import           Data.Text.Encoding           (decodeUtf8)
import           Data.Word
import           Network.Socket.ByteString    (recv, send)
import           System.FilePath

import           Test.Hspec
import           Test.Hspec.Contrib.HUnit
import           Test.Hspec.QuickCheck
import           Test.HUnit
import           Test.QuickCheck              hiding (Result)

import           Rho.InfoHash
import           Rho.Listener                 hiding (listener)
import           Rho.PeerComms.Handshake
import           Rho.PeerComms.Message
import           Rho.PeerComms.PeerConnection
import           Rho.PeerComms.PeerId
import           Rho.Session
import           Rho.TestUtils

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
      let
        ret = mkPeerMsg defaultMsgTable msg >>= parsePeerMsg
        ex  = unlines [ "expected: " ++ (show :: Either B.ByteString PeerMsg -> String) (Right msg)
                      , "found: " ++ show ret ]
      in
        counterexample ex (ret == Right msg)

    fromHUnitTest recvMessageRegression

    modifyMaxSuccess (const 10) $ prop "recvMessages delivers" $ \(PFXd msgs) ->
      ioProperty $ do
        emitter  <- mkMessageEmitter msgs
        listener <- initListener emitter
        recvd    <- map unwrapRecvd `fmap` replicateM (length msgs) (recvMessage listener)
        return $ msgs == recvd

    fromHUnitTest parseLongMsg
    fromHUnitTest parseExtendedHsAndBF
    fromHUnitTest parsePieceReqs

    -- captured messages from a uTorrent <-> Transmission channel
    -- (BEP 6 - Fast extension messages are removed, we don't support it yet)
    fromHUnitTest utorrentExample
    fromHUnitTest transmissionExample

    fromHUnitTest regression1

    modifyMaxSuccess (const 10) $ prop "recvMessage using socket delivers" $
      \(PFXd msgs) -> ioProperty $ do
        (sock1, sock2) <- initConnectedSocks
        listener <- initListener (recv sock2 4096)

        action1 <- async $ forM_ msgs (send sock1)
        action2 <- async $ replicateM (length msgs) (recvMessage listener)

        wait action1
        recvd <- wait action2
        return $ map unwrapRecvd recvd == msgs

unwrapRecvd :: RecvMsg -> B.ByteString
unwrapRecvd (ConnClosed bs) = bs
unwrapRecvd (Msg bs) = bs

parseLongMsg :: Test
parseLongMsg = TestLabel "parsing long message (using listener, starting with handshake)" $
  TestCase $ do
    extendedMsg <- B.readFile (dataRoot </> "extended_msg")
    emitter <- mkByteEmitter extendedMsg
    listener <- initListener emitter
    _ <- recvAndParseHs listener
    (extendedHs : _) <- recvAndParse listener 26
    case extendedHs of
      Extended (ExtendedHandshake _ _ hsData) -> do
        assertEqual "wrong `v` field" (Just "µTorrent 1.7.7") (decodeUtf8 <$> ehdV hsData)
        assertEqual "wrong `reqq` field" Nothing (ehdReqq hsData)
      notHs -> assertFailure $ "message is not extended handshake: " ++ show notHs
    checkBuffer listener

parseExtendedHsAndBF :: Test
parseExtendedHsAndBF = TestLabel "parsing extended handshake followed by bitfield" $ TestCase $ do
  msg <- B.readFile (dataRoot </> "extended_hs_with_bf")
  emitter <- mkByteEmitter msg
  listener <- initListener emitter
  (extendedHs : _) <- recvAndParse listener 2
  case extendedHs of
    Extended (ExtendedHandshake msgTbl msgData hsData) -> do
      assertEqual "wrong `v` field" (Just "Transmission 2.84") (ehdV hsData)
      assertEqual "wrong `reqq` field" (Just 512) (ehdReqq hsData)
      case M.lookup UtMetadata msgTbl of
        Nothing -> assertFailure "can't find UtMetadata in message table"
        Just i -> assertEqual "ut_metadata id is wrong" 3 i
      case find (\case { UtMetadataSize _ -> True; _ -> False }) msgData
             >>= \(UtMetadataSize i) -> return i of
        Just size -> assertEqual "metadata size is wrong" 7778 size
        Nothing -> assertFailure "can't find metadata size"
    notHs -> assertFailure $ "message is not extended handshake: " ++ show notHs
  checkBuffer listener

parsePieceReqs :: Test
parsePieceReqs = TestLabel "parsing piece requests" $ TestCase $ do
  msg <- B.readFile (dataRoot </> "piece_requests")
  emitter <- mkByteEmitter msg
  listener <- initListener emitter
  _ <- recvAndParse listener 2
  checkBuffer listener

utorrentExample :: Test
utorrentExample = TestLabel "parsing utorrent leecher example" $
  TestCase $ do
    extendedMsg <- B.readFile (dataRoot </> "utorrent_example")
    emitter <- mkByteEmitter extendedMsg
    listener <- initListener emitter
    _ <- recvAndParseHs listener
    _ <- recvAndParse listener 3
    checkBuffer listener

transmissionExample :: Test
transmissionExample = TestLabel "parsing transmission seeder example" $
  TestCase $ do
    extendedMsg <- B.readFile (dataRoot </> "transmission_example")
    emitter <- mkByteEmitter extendedMsg
    listener <- initListener emitter
    _ <- recvAndParseHs listener
    _ <- recvAndParse listener 3
    checkBuffer listener

regression1 :: Test
regression1 = TestLabel "regression -- parsing series of messages" $
  TestCase $ do
    msg <- B.readFile (dataRoot </> "regression1")
    let (firstMsg, rest) = B.splitAt 68 msg
        (secondMsg, rest') = B.splitAt 124 rest
        (thirdMsg, rest'') = B.splitAt 5 rest'
    assertBool ("message splitted wrong: " ++ show (B.unpack rest''))
               (B.null rest'' && (firstMsg <> secondMsg <> thirdMsg == msg))
    emitter <- mkMessageEmitter [firstMsg, secondMsg, thirdMsg]
    listener <- initListener emitter
    _ <- recvAndParseHs listener
    msgs <- replicateM 3 (recvMessage listener)
    let ms = mapMaybe (\m -> case m of { Msg m' -> Just m'; _ -> Nothing }) msgs
    case map parsePeerMsg ms of
      [Right (Extended ExtendedHandshake{}), Right Bitfield{}, Right Unchoke{}] -> return ()
      w -> assertFailure ("messages are parsed wrong: " ++ show w)
    checkBuffer listener

recvMessageRegression :: Test
recvMessageRegression = TestLabel "regression -- recvMessage sometimes returns wrong" $
  TestCase $ do
    let first = [0,0,0,1,1]
        second = [0,0,0,0]
        msgs = [B.pack first, B.pack second]
    emitter <- mkMessageEmitter msgs
    listener <- initListener emitter
    msg1 <- recvMessage listener
    msg2 <- recvMessage listener
    assertEqual "first message is wrong" first (B.unpack $ unwrapRecvd msg1)
    assertEqual "second message is wrong" second (B.unpack $ unwrapRecvd msg2)
    checkBuffer listener

recvAndParseHs :: Listener -> Assertion' Handshake
recvAndParseHs listener = do
    hsMsg <- recvHandshake listener
    case hsMsg of
      ConnClosed _ -> assertFailure' "Receiving handshake failed"
      Msg hs ->
        case parseHandshake hs of
          Left err  -> assertFailure' $ "Parsing handshake failed: " ++ err
          Right hs' -> return hs'

recvAndParse :: Listener -> Int -> Assertion' [PeerMsg]
recvAndParse listener n = do
  msgs <- replicateM n (recvMessage listener)
  let ms = mapMaybe (\msg -> case msg of { Msg m -> Just m; _ -> Nothing }) msgs
  assertEqual "Failed to read some messages." n (length ms)
  parseMsgs parsePeerMsg ms

checkBuffer :: Listener -> Assertion
checkBuffer Listener{deque=buf} = do
    (d, l) <- readIORef buf
    unless (D.null d) $ do
      let bufContents = mconcat $ D.takeFront (D.length d) d
      assertFailure $ "Buffer is not empty: " ++ show bufContents
    assertEqual "Buffer length is not zero" 0 l

parseMsgs :: (B.ByteString -> Either String a) -> [B.ByteString] -> Assertion' [a]
parseMsgs _ [] = return []
parseMsgs p (m : ms) = do
    case p m of
      Left err -> assertFailure' $ "Failed to parse msg: " ++ show (B.unpack m) ++ " (" ++ err ++ ")"
      Right m' -> do
        ms' <- parseMsgs p ms
        return $ m' : ms'

recvMsg :: RecvMsg -> Either B.ByteString B.ByteString
recvMsg (ConnClosed bs) = Left bs
recvMsg (Msg bs) = Right bs


-- * Arbitrary stuff

newtype PFXd = PFXd [B.ByteString] deriving (Eq)

instance Arbitrary PFXd where
    arbitrary = PFXd <$> listOf (do
      -- longest message will probably be the metadata data message:
      -- 16kb piece data + meta info. we generate 32kb messages here.
      len <- choose (0, 2 ^ (15 :: Word16))
      msg <- replicateM (fromIntegral len) arbitrary
      return $ B.pack [0, 0] <> (LB.toStrict . BB.toLazyByteString . BB.word16BE $ len) <> B.pack msg)

    shrink _ = []

instance Show PFXd where
    show (PFXd bs) = show $ map B.unpack bs

genBytes :: Int -> Gen B.ByteString
genBytes n = B.pack `fmap` replicateM n arbitrary

instance Arbitrary InfoHash where
    arbitrary = InfoHash `fmap` genBytes 20
    shrink _  = []

instance Arbitrary PeerId where
    arbitrary = PeerId `fmap` genBytes 20
    shrink _  = []

instance Arbitrary PeerMsg where
    arbitrary = oneof
      [ return KeepAlive
      , return Choke
      , return Interested
      , return NotInterested
      , Have <$> arbitrary
      , Bitfield <$> (genBytes =<< choose (0, 50))
      , Request <$> arbitrary <*> arbitrary <*> arbitrary
      , Piece <$> arbitrary <*> arbitrary <*> genBytes 20
      , Cancel <$> arbitrary <*> arbitrary <*> arbitrary
      -- TODO: fix this
      , Port . fromIntegral <$> (arbitrary :: Gen Word16)
      , Extended <$> arbitrary
      ]

    shrink _ = [] -- TODO: maybe implement this

instance Arbitrary ExtendedPeerMsg where
    arbitrary = oneof
      [ genExtendedHandshake
      , MetadataRequest <$> arbitrary
      , MetadataData <$> arbitrary <*> arbitrary <*> genBytes 10
      , MetadataReject <$> arbitrary
      ]

    shrink _ = []

genExtendedHandshake :: Gen ExtendedPeerMsg
genExtendedHandshake = do
    msgTable <- genExtendedPeerMsgTable
    msgData <- genExtendedMsgTypeData msgTable
    hsData <- genExtendedHsData
    return $ ExtendedHandshake msgTable msgData hsData
  where
    genExtendedPeerMsgTable :: Gen ExtendedPeerMsgTable
    genExtendedPeerMsgTable = M.fromList <$> oneof
      [ return []
      , do i <- arbitrary
           return [(UtMetadata, i)]
      ]

    genExtendedMsgTypeData :: ExtendedPeerMsgTable -> Gen [ExtendedMsgTypeData]
    genExtendedMsgTypeData tbl
      -- we only generate UtMetadataSize when we have UtMetadata in `m`
      -- dictionary
      | UtMetadata `M.member` tbl = do
          i <- arbitrary
          return [UtMetadataSize i]
      | otherwise = return []

    genExtendedHsData :: Gen ExtendedHandshakeData
    genExtendedHsData =
      ExtendedHandshakeData <$> genMaybe (genBytes 5) <*> genMaybe arbitrary
