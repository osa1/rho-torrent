{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Rho.PeerCommsSpec where

import           Control.Applicative
import           Control.Monad
import qualified Data.ByteString              as B
import qualified Data.Dequeue                 as D
import           Data.IORef
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           System.FilePath

import           Test.Hspec
import           Test.Hspec.HUnit
import           Test.Hspec.QuickCheck
import           Test.HUnit
import           Test.QuickCheck              hiding (Result)

import qualified Rho.Bitfield                 as BF
import           Rho.InfoHash
import           Rho.Listener
import qualified Rho.ListenerSpec             as LS
import           Rho.PeerComms
import           Rho.PeerComms.Handshake
import           Rho.PeerComms.Message
import           Rho.PeerComms.PeerConnection

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

    fromHUnitTest recvMessageRegression

    -- QuickCheck sucks. It doesn't prove any easy ways to report failure
    -- resons(unlike HUnit) and forces us to define lots of newtypes and
    -- Arbitrary instances.
    modifyMaxSuccess (const 100) $ prop "recvMessages receives correct lengths" $ \(PFXd msgs) ->
      ioProperty $ do
        emitter <- LS.mkMessageEmitter msgs
        listener <- initListener emitter
        recvd <- map unwrapRecvd `fmap` replicateM (length msgs) (recvMessage listener)
        return ( length recvd == length msgs && ll recvd == ll msgs )

    fromHUnitTest parseLongMsg
    fromHUnitTest parseExtendedHsAndBF
    fromHUnitTest parsePieceReqs

    -- captured messages from a uTorrent <-> Transmission channel
    -- (BEP 6 - Fast extension messages are removed, we don't support it yet)
    fromHUnitTest utorrentExample
    fromHUnitTest transmissionExample

    fromHUnitTest regression1

-- FIXME: copied from ListenerSpec, maybe move to some shared module
ll :: [B.ByteString] -> Int
ll = foldl' (\acc b -> acc + B.length b) 0

unwrapRecvd :: RecvMsg -> B.ByteString
unwrapRecvd (ConnClosed bs) = bs
unwrapRecvd (Msg bs) = bs

parseLongMsg :: Test
parseLongMsg = TestLabel "parsing long message (using listener, starting with handshake)" $
  TestCase $ do
    extendedMsg <- B.readFile (dataRoot </> "extended_msg")
    emitter <- mkMessageEmitter extendedMsg
    listener <- initListener emitter
    recvAndParseHs listener
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

utorrentExample :: Test
utorrentExample = TestLabel "parsing utorrent leecher example" $
  TestCase $ do
    extendedMsg <- B.readFile (dataRoot </> "utorrent_example")
    emitter <- mkMessageEmitter extendedMsg
    listener <- initListener emitter
    recvAndParseHs listener
    recvAndParse listener 3
    checkBuffer listener

transmissionExample :: Test
transmissionExample = TestLabel "parsing transmission seeder example" $
  TestCase $ do
    extendedMsg <- B.readFile (dataRoot </> "transmission_example")
    emitter <- mkMessageEmitter extendedMsg
    listener <- initListener emitter
    recvAndParseHs listener
    recvAndParse listener 3
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
    emitter <- LS.mkMessageEmitter [firstMsg, secondMsg, thirdMsg]
    listener <- initListener emitter
    recvAndParseHs listener
    msgs <- replicateM 3 (recvMessage listener)
    let ms = mapMaybe (\msg -> case msg of { Msg m -> Just m; _ -> Nothing }) msgs
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
    emitter <- LS.mkMessageEmitter msgs
    listener <- initListener emitter
    msg1 <- recvMessage listener
    msg2 <- recvMessage listener
    assertEqual "first message is wrong" first (B.unpack $ unwrapRecvd msg1)
    assertEqual "second message is wrong" second (B.unpack $ unwrapRecvd msg2)
    checkBuffer listener

recvAndParseHs :: Listener -> Assertion
recvAndParseHs listener = do
    hsMsg <- recvHandshake listener
    case hsMsg of
      ConnClosed _ -> assertFailure "Receiving handshake failed"
      Msg hs ->
        case parseHandshake hs of
          Left err -> assertFailure $ "Parsing handshake failed: " ++ err
          Right _ -> return ()

recvAndParse :: Listener -> Int -> Assertion
recvAndParse listener n = do
  msgs <- replicateM n (recvMessage listener)
  let ms = mapMaybe (\msg -> case msg of { Msg m -> Just m; _ -> Nothing }) msgs
  assertEqual "Failed to read some messages." n (length ms)
  parseMsgs parsePeerMsg ms

checkBuffer :: Listener -> Assertion
checkBuffer (Listener buf _ _ _ _) = do
    (d, l) <- readIORef buf
    unless (D.null d) $ do
      let bufContents = mconcat $ D.takeFront (D.length d) d
      assertFailure $ "Buffer is not empty: " ++ show bufContents
    assertEqual "Buffer length is not zero" 0 l

parseMsgs :: (B.ByteString -> Either String a) -> [B.ByteString] -> Assertion
parseMsgs _ [] = return ()
parseMsgs p (m : ms) = do
    case p m of
      Left err -> assertFailure $ "Failed to parse msg: " ++ show (B.unpack m) ++ " (" ++ err ++ ")"
      Right _ -> parseMsgs p ms

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

newtype PFXd = PFXd [B.ByteString] deriving (Eq)

instance Arbitrary PFXd where
    arbitrary = PFXd <$> (listOf $ do
      len <- arbitrary
      msg <- replicateM (fromIntegral len) arbitrary
      return . B.pack $ [0, 0, 0, len] ++ msg)

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
