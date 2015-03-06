{-# LANGUAGE OverloadedStrings #-}

module Rho.ClientSpec where

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad
import qualified Data.BEncode                as BE
import qualified Data.ByteString.Char8       as BC
import qualified Data.ByteString.Lazy        as LB
import           Data.Either
import           Data.IORef
import qualified Data.Map                    as M
import           Data.Maybe
import qualified Data.Set                    as S
import           Data.Word
import           Network.Socket
import           System.Directory
import           System.FilePath
import           System.Process
import           System.Timeout

import           Test.Hspec
import           Test.Hspec.Contrib.HUnit
import           Test.HUnit

import           Rho.Magnet
import           Rho.Metainfo
import           Rho.MetainfoSpec            (parseMIAssertion)
import           Rho.PeerComms.Message
import           Rho.PeerComms.PeerConnState
import           Rho.PeerComms.PeerId
import           Rho.PieceMgr
import           Rho.Session
import           Rho.SessionState
import           Rho.TestTracker
import           Rho.Tracker
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
      fromHUnitTest $ TestLabel "deadlock regression" deadlockTest

connectTest :: Test
connectTest = TestCase $ do
    pwd <- getCurrentDirectory
    hostAddr <- inet_addr "127.0.0.1"
    let sockAddr = SockAddrInet (fromIntegral (6969 :: Word16)) hostAddr

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

    tracker <- spawnTracker pwd []
    threadDelay 500000

    udpComms <- initUDPCommHandler
    scrapeRet <- scrapeRequestUDP udpComms sockAddr [iHash $ mInfo mi]
    case scrapeRet of
      Left err -> assertFailure $ "Can't scrape: " ++ err
      Right _  -> return ()

    terminateProcess tracker

metadataTransferTest :: Test
metadataTransferTest = TestCase $ do
    -- FIXME: We're having a race conditions here -- when the tracker
    -- returns two peers to both peers, both peers get stuck.
    opentracker <- spawnTracker "tests/should_parse/" []
    let ts = [UDPTracker "127.0.0.1" (fromIntegral 6969)]
    Metainfo{mInfo=info} <- parseMIAssertion "tests/should_parse/archlinux-2014.11.01-dual.iso.torrent"
    let infoSize = fromIntegral $ LB.length $ BE.encode info
        pid1     = mkPeerId 1
        pid2     = mkPeerId 2
        hash     = iHash info
        magnet   = Magnet hash ts Nothing
    clientWInfo   <- initTorrentSession info ts pid1
    checkMIPieceMgrInit clientWInfo
    checkMIPieceMgrMissings "clientWInfo" clientWInfo
    magnetComplete <- newEmptyMVar
    let magnetCompleteAction = putMVar magnetComplete ()
    clientWMagnet <- initMagnetSession magnet pid2
    modifyMVar_ (sessOnMIComplete clientWMagnet) (\_ -> return magnetCompleteAction)

    _seederThread <- async $ runTorrentSession clientWInfo info
    threadDelay (1 * 1000000)
    _leecherThread <- async $ runMagnetSession clientWMagnet

    threadDelay (3 * 1000000)
    checkConnectedPeer "clientWInfo" clientWInfo
    checkConnectedPeer "clientWMagnet" clientWMagnet

    checkExtendedMsgTbl "clientWInfo" clientWInfo
    checkExtendedMsgTbl "clientWMagnet" clientWMagnet
    -- clientWMagnet should have connected with clientWInfo and
    -- clientWInfo's `pcMetadataSize` should be set
    checkPeerForMI infoSize clientWMagnet
    -- clientWMagnet's metainfo piece manager should be initialized
    checkMIPieceMgrInit clientWMagnet

    -- check this again for two reasons:
    -- * sometimes peers handshake with themselves (e.g. when tracker
    --   returns the peer that requested peers)
    -- * when P1 sends handshake to P2 and at the same time P2 sends
    --   a handshake to P1. sometimes they establish two connections.
    checkConnectedPeer "clientWInfo" clientWInfo
    checkConnectedPeer "clientWMagnet" clientWMagnet
    checkMIPieceMgrMissings "clientWMagnet" clientWMagnet
    checkCallbackCalled magnetComplete

    terminateProcess opentracker
  where
    checkExtendedMsgTbl :: String -> Session -> Assertion
    checkExtendedMsgTbl info Session{sessPeers=peers} = do
      ps' <- M.elems <$> readMVar peers
      case ps' of
        [peerWInfo] -> do
          p <- readIORef peerWInfo
          assertEqual ("wrong extended msg tbl(" ++ info ++ ")") defaultMsgTable
                      (pcExtendedMsgTbl p)
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
      when isEmpty $ assertFailure "Metainfo downloaded callback is not called"

torrentTransferTest :: Test
torrentTransferTest = TestCase $ do
    opentracker <- spawnTracker "tests/should_parse/" []
    let ts = [UDPTracker "127.0.0.1" (fromIntegral 6969)]
    pwd <- getCurrentDirectory
    Metainfo{mInfo=info} <- parseMIAssertion (pwd </> "test/test.torrent")
    let pid1 = mkPeerId 1
        pid2 = mkPeerId 2

    -- setup seeder
    seeder <- initTorrentSession info ts pid1
    modifyMVar_ (sessPieceMgr seeder) $ \_ -> (Just . fst) <$> tryReadFiles info "test"
    checkPiecesComplete (sessPieceMgr seeder)
    checkDownloadedZero seeder
    seederThread <- async $ runTorrentSession seeder info

    -- make sure the seeder established a connection with the tracker
    threadDelay 300000

    -- setup leecher
    leecher <- initMagnetSession (Magnet (iHash info) ts Nothing) pid2

    ret <- timeout (20 * 1000000) (runMagnetSession leecher)
    cancel seederThread
    case ret of
      Nothing -> assertFailure "Leecher timed out"
      Just b  -> assertBool "Failed to download the torrent in time" b

    checkDownloaded leecher
    checkUploaded seeder

    -- at this point the torrent files should have been generated and we
    -- should be able to load those files to a piece manager for seeding
    (_, b) <- tryReadFiles info pwd
    assertBool "Can't load downloaded files to piece manager" b

    -- remove downloaded files
    removeDirectoryRecursive (pwd </> "seed_files")

    terminateProcess opentracker
  where
    checkPiecesComplete :: MVar (Maybe PieceMgr) -> Assertion
    checkPiecesComplete var = do
      pmgr <- readMVar var
      case pmgr of
        Nothing -> assertFailure "Piece manager is not initialized"
        Just ps -> do
          missings <- missingPieces ps
          assertEqual "Piece manager has missing pieces" [] missings

    checkDownloadedZero :: Session -> Assertion
    checkDownloadedZero sess = do
      (d, l, _) <- stats sess
      assertEqual "downloaded is wrong" 0 d
      pm <- fromJust <$> readMVar (sessPieceMgr sess)
      assertEqual "left is wrong" (pmTotalSize pm) l

    checkDownloaded :: Session -> Assertion
    checkDownloaded sess = do
      (d, l, _) <- stats sess
      pm <- fromJust <$> readMVar (sessPieceMgr sess)
      assertEqual "downloaded is wrong" (pmTotalSize pm) d
      assertEqual "left is wrong" 0 l

    checkUploaded :: Session -> Assertion
    checkUploaded sess = do
      (_, _, u) <- stats sess
      pm <- fromJust <$> readMVar (sessPieceMgr sess)
      assertEqual "uploaded is wrong" (pmTotalSize pm) u

deadlockTest :: Test
deadlockTest = TestCase $ do
    (tr, SockAddrInet pn _) <- runTracker
    localhost <- inet_addr "127.0.0.1"
    let ts = [UDPTracker "127.0.0.1" pn]
    Metainfo{mInfo=info} <- parseMIAssertion "test/test.torrent"
    let pid1     = mkPeerId 1
        pid2     = mkPeerId 2
        hash     = iHash info
        magnet   = Magnet hash ts Nothing
    clientWInfo   <- initTorrentSession info ts pid1
    magnetComplete <- newEmptyMVar
    let magnetCompleteAction = putMVar magnetComplete ()
    clientWMagnet <- initMagnetSession magnet pid2
    modifyMVar_ (sessOnMIComplete clientWMagnet) (\_ -> return magnetCompleteAction)

    let client1Port = sessPort clientWInfo
        client2Port = sessPort clientWMagnet

    modifyMVar_ (connectedClients tr) $ \s ->
      return $ S.insert (SockAddrInet client1Port localhost) $
                 S.insert (SockAddrInet client2Port localhost) s

    _seederThread <- async $ runTorrentSession clientWInfo info
    _leecherThread <- async $ runMagnetSession clientWMagnet

    threadDelay (3 * 1000000)
    checkConnectedPeer "clientWInfo" clientWInfo
    checkConnectedPeer "clientWMagnet" clientWMagnet

    return ()

checkConnectedPeer :: String -> Session -> Assertion
checkConnectedPeer info Session{sessPeers=peers} = do
  ps' <- M.elems <$> readMVar peers
  assertEqual ("connected to wrong number of clients(" ++ info ++ ")") 1 (length ps')

spawnTracker :: FilePath -> [String] -> IO ProcessHandle
spawnTracker pwd args = do
    (_, _, _, handle) <- createProcess (proc "opentracker" args){cwd=Just pwd}
    return handle

mkPeerId :: Int -> PeerId
mkPeerId i =
    let str = show i
    in PeerId $ BC.pack $ replicate (20 - length str) '0' ++ str
