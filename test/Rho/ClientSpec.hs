module Rho.ClientSpec where

import           Control.Applicative
import           Control.Concurrent
import qualified Data.BEncode                 as BE
import qualified Data.ByteString              as B
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
    -- mi <- parseMetainfo <$> B.readFile "test/test.torrent"
    mi <- parseMetainfo <$> B.readFile "tests/should_parse/archlinux-2014.11.01-dual.iso.torrent"
    case mi of
      Left err -> assertFailure $ "Can't parse test.torrent: " ++ err
      Right Metainfo{mInfo=info} -> do
        let infoSize = fromIntegral $ LB.length $ BE.encode info
            pid1     = PeerId $ B.pack $ replicate 19 0 ++ [1]
            pid2     = PeerId $ B.pack $ replicate 19 0 ++ [2]
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
          Right Supports      -> do
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
  return ()

spawnTracker :: FilePath -> [String] -> IO ProcessHandle
spawnTracker pwd args = do
    (_, _, _, handle) <- createProcess (proc "opentracker" args){cwd=Just pwd}
    return handle
