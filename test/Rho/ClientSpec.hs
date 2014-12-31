module Rho.ClientSpec where

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad
import qualified Data.BEncode                 as BE
import qualified Data.ByteString.Char8        as BC
import qualified Data.ByteString.Lazy         as LB
import           Data.Either
import           Data.IORef
import qualified Data.Map                     as M
import           Data.Maybe
import           Data.Word
import           Network.Socket
import           System.Directory
import           System.FilePath
import           System.Process

import           Test.Hspec
import           Test.Hspec.Contrib.HUnit
import           Test.HUnit

import           Rho.Magnet
import           Rho.Metainfo
import           Rho.MetainfoSpec             (parseMIAssertion)
import           Rho.PeerComms.Handshake
import           Rho.PeerComms.Message
import           Rho.PeerComms.PeerConnection
import           Rho.PeerComms.PeerConnState
import           Rho.PeerComms.PeerId
import           Rho.PieceMgr
import           Rho.Session
import           Rho.SessionState
import           Rho.TrackerComms.UDP

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "client functions" $ do
      fromHUnitTest $ TestLabel "connecting" connectTest
      fromHUnitTest $ TestLabel "scraping" scrapeTest
      fromHUnitTest $ TestLabel "metadata transfer" metadataTransferTest
      fromHUnitTest $ TestLabel "torrent transfer" torrentTransferTest

connectTest :: Test
connectTest = TestCase $ do
    pwd <- getCurrentDirectory
    hostAddr <- inet_addr "127.0.0.1"
    let sockAddr = SockAddrInet (fromIntegral (6969 :: Word16)) hostAddr

    putStrLn "Spawning tracker"
    tracker <- spawnTracker pwd []
    threadDelay 500000

    udpComms <- initUDPCommHandler
    ret <- connectRequest udpComms sockAddr
    assertBool "Peer can't connect" $ isRight ret
    terminateProcess tracker

scrapeTest :: Test
scrapeTest = TestCase $ do
    pwd <- getCurrentDirectory
    mi  <- parseMIAssertion (pwd </> "test/test.torrent")

    hostAddr <- inet_addr "127.0.0.1"
    let sockAddr = SockAddrInet (fromIntegral (6969 :: Word16)) hostAddr

    putStrLn "Spawning tracker"
    tracker <- spawnTracker pwd []
    threadDelay 500000

    udpComms <- initUDPCommHandler
    scrapeRet <- scrapeRequestUDP udpComms sockAddr [iHash $ mInfo mi]
    case scrapeRet of
      Left err -> assertFailure $ "Can't scrape: " ++ err
      Right sr -> putStrLn $ "Scrape result: " ++ show sr

    terminateProcess tracker

metadataTransferTest :: Test
metadataTransferTest = TestCase $ do
    Metainfo{mInfo=info} <- parseMIAssertion "tests/should_parse/archlinux-2014.11.01-dual.iso.torrent"
    let infoSize = fromIntegral $ LB.length $ BE.encode info
        pid1     = mkPeerId 1
        pid2     = mkPeerId 2
        hash     = iHash info
        magnet   = Magnet hash [] Nothing
    localhost     <- inet_addr "127.0.0.1"
    clientWInfo   <- initTorrentSession' localhost info pid1
    checkMIPieceMgrInit clientWInfo
    checkMIPieceMgrMissings "clientWInfo" clientWInfo
    magnetComplete <- newEmptyMVar
    let magnetCompleteAction = putMVar magnetComplete ()
    clientWMagnet <-
      initMagnetSession' localhost magnet pid2
    modifyMVar_ (sessOnMIComplete clientWMagnet) (\_ -> return magnetCompleteAction)
    threadDelay 100000
    hsResult <- handshake clientWMagnet (SockAddrInet (sessPort clientWInfo) localhost) hash
    -- hsResult <- handshake clientWInfo (SockAddrInet (sessPort clientWMagnet) localhost) hash
    threadDelay 100000
    case hsResult of
      Left err            -> assertFailure $ "Handshake failed: " ++ err
      Right DoesntSupport -> assertFailure "Wrong extended message support"
      Right Supports      -> return ()

    checkConnectedPeer "clientWInfo" clientWInfo
    checkConnectedPeer "clientWMagnet" clientWMagnet

    checkExtendedMsgTbl "clientWInfo" clientWInfo
    checkExtendedMsgTbl "clientWMagnet" clientWMagnet
    -- clientWMagnet should have connected with clientWInfo and
    -- clientWInfo's `pcMetadataSize` should be set
    checkPeerForMI infoSize clientWMagnet
    -- clientWMagnet's metainfo piece manager should be initialized
    checkMIPieceMgrInit clientWMagnet

    miPieces <- fromJust <$> (tryReadMVar $ sessMIPieceMgr clientWMagnet)
    sendMetainfoRequests (sessPeers clientWMagnet) miPieces
    threadDelay 100000
    checkMIPieceMgrMissings "clientWMagnet" clientWMagnet
    checkCallbackCalled magnetComplete
  where
    checkConnectedPeer :: String -> Session -> Assertion
    checkConnectedPeer info Session{sessPeers=peers} = do
      ps' <- M.elems <$> readMVar peers
      assertEqual ("connected to wrong number of clients(" ++ info ++ ")") 1 (length ps')

    checkExtendedMsgTbl :: String -> Session -> Assertion
    checkExtendedMsgTbl info Session{sessPeers=peers} = do
      ps' <- M.elems <$> readMVar peers
      case ps' of
        [peerWInfo] -> do
          p <- readIORef peerWInfo
          assertEqual ("wrong extended msg tbl(" ++ info ++ ")") defaultMsgTable (pcExtendedMsgTbl p)
        _ -> assertFailure $
               "Connected to wrong number of clients(" ++ info ++ "): " ++ show (length ps')

    checkPeerForMI :: Word64 -> Session -> Assertion
    checkPeerForMI infoSize Session{sessPeers=peers} = do
      ps' <- M.elems <$> readMVar peers
      case ps' of
        [peerWInfo] -> do
          p <- readIORef peerWInfo
          assertEqual "peerWInfo pcMetadataSize is wrong" (Just infoSize) (pcMetadataSize p)
        _ -> assertFailure $ "Connected to wrong number of clients: " ++ show (length ps')

    checkMIPieceMgrInit :: Session -> Assertion
    checkMIPieceMgrInit Session{sessMIPieceMgr=mi} = do
      miInit <- not <$> isEmptyMVar mi
      assertBool "sessMIPieceMgr is not initialized" miInit

    checkMIPieceMgrMissings :: String -> Session -> Assertion
    checkMIPieceMgrMissings info Session{sessMIPieceMgr=mi} = do
      mi' <- tryReadMVar mi
      case mi' of
        Nothing -> assertFailure $ "session manager is not initialized(" ++ info ++ ")"
        Just mi'' -> do
          missings <- missingPieces mi''
          assertEqual ("some pieces are missings(" ++ info ++ ")") [] missings

    checkCallbackCalled :: MVar () -> Assertion
    checkCallbackCalled var = do
      isEmpty <- isEmptyMVar var
      if isEmpty then assertFailure "Metainfo downloaded callback is not called" else return ()

torrentTransferTest :: Test
torrentTransferTest = TestCase $ do
    pwd <- getCurrentDirectory
    Metainfo{mInfo=info, mAnnounce=ann} <- parseMIAssertion (pwd </> "test/test.torrent")
    let pid1 = mkPeerId 1
        pid2 = mkPeerId 2

    -- setup tracker
    tracker <- spawnTracker pwd []

    -- setup seeder
    seeder <- initTorrentSession info pid1
    modifyMVar_ (sessPieceMgr seeder) $ \_ -> (Just . fst) <$> tryReadFiles info "test"
    checkPiecesComplete (sessPieceMgr seeder)
    seederThread <- async $ runTorrentSession seeder [ann] info

    -- make sure the seeder established a connection with the tracker
    threadDelay 500000

    -- setup leecher
    leecher <- initTorrentSession info pid2
    modifyMVar_ (sessPieceMgr leecher) $ \_ ->
      Just <$> newPieceMgr (torrentSize info) (iPieceLength info)
    checkPiecesMissing (sessPieceMgr leecher)
    torrentDone <- newEmptyMVar
    torrentDoneThread <- async $ readMVar torrentDone
    modifyMVar_ (sessOnTorrentComplete leecher) $ \_ -> return $ putMVar torrentDone ()
    leecherThread <- async $ runTorrentSession leecher [ann] info

    -- for some reason, opentracker returning weird port address(0) to the
    -- peers and they can't establish a connection because of that. so we
    -- manually introduce the peers.
    let seederPort = sessPort seeder
    localhost <- inet_addr "127.0.0.1"
    hsResult <- handshake leecher (SockAddrInet seederPort localhost) (iHash info)
    case hsResult of
      Left err            -> assertFailure $ "Handshake failed: " ++ err
      Right DoesntSupport -> assertFailure "Wrong extended message support"
      Right Supports      -> return ()

    timeoutThread <- async $ threadDelay (10 * 1000000)
    void $ waitAnyCancel [seederThread, leecherThread, torrentDoneThread, timeoutThread]
    terminateProcess tracker

    notDone <- isEmptyMVar torrentDone
    assertBool "Failed to download the torrent in time" (not notDone)
  where
    checkPiecesComplete :: MVar (Maybe PieceMgr) -> Assertion
    checkPiecesComplete var = do
      pmgr <- readMVar var
      case pmgr of
        Nothing -> assertFailure "Piece manager is not initialized"
        Just ps -> do
          missings <- missingPieces ps
          assertEqual "Piece manager has missing pieces" [] missings

    checkPiecesMissing :: MVar (Maybe PieceMgr) -> Assertion
    checkPiecesMissing var = do
      pmgr <- readMVar var
      case pmgr of
        Nothing -> assertFailure "Piece manager is not initialized"
        Just ps -> do
          missings <- missingPieces ps
          assertBool "Piece manager doesn't have missing pieces" (not $ null missings)

spawnTracker :: FilePath -> [String] -> IO ProcessHandle
spawnTracker pwd args = do
    (_, _, _, handle) <- createProcess (proc "opentracker" args){cwd=Just pwd}
    return handle

mkPeerId :: Int -> PeerId
mkPeerId i =
    let str = show i
    in PeerId $ BC.pack $ replicate (20 - length str) '0' ++ str
