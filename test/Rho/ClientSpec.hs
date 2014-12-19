module Rho.ClientSpec where

import           Control.Applicative
import           Control.Concurrent
import qualified Data.BEncode                as BE
import qualified Data.ByteString             as B
import qualified Data.ByteString.Lazy        as LB
import           Data.Either
import           Data.IORef
import qualified Data.Map                    as M
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
import           Rho.PeerComms.Handshake
import           Rho.PeerComms.Message
import           Rho.PeerComms.PeerConnState
import           Rho.Session
import           Rho.SessionState
import           Rho.TrackerComms.UDP

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "client functions" $ do
      fromHUnitTest $ TestLabel "connecting" connectTest
      fromHUnitTest $ TestLabel "metadata transfer" metadataTransferTest

connectTest :: Test
connectTest = TestCase $ do
    pwd <- getCurrentDirectory
    let torrentPath = pwd </> "test/test.torrent"
    torrentContents <- B.readFile torrentPath
    case parseMetainfo torrentContents of
      Left err -> assertFailure $ "Failed to parse torrent: " ++ err
      Right Metainfo{} -> do
        hostAddr <- inet_addr "127.0.0.1"
        let sockAddr = SockAddrInet (fromIntegral (6969 :: Word16)) hostAddr

        putStrLn "Spawning tracker"
        tracker <- spawnTracker pwd []
        threadDelay 500000

        udpComms <- initUDPCommHandler
        ret <- connectRequest udpComms sockAddr
        assertBool "Peer can't connect" $ isRight ret
        terminateProcess tracker

metadataTransferTest :: Test
metadataTransferTest = TestCase $ do
    mi <- parseMetainfo <$> B.readFile "test/test.torrent"
    case mi of
      Left err -> assertFailure $ "Can't parse test.torrent: " ++ err
      Right Metainfo{mInfo=info} -> do
        let infoSize = fromIntegral $ LB.length $ BE.encode info
            pid1     = PeerId $ B.pack $ replicate 19 0 ++ [1]
            pid2     = PeerId $ B.pack $ replicate 19 0 ++ [2]
            port1    = fromIntegral (5445 :: Word16)
            port2    = fromIntegral (5446 :: Word16)
            hash     = iHash info
            magnet   = Magnet hash [] Nothing
        localhost     <- inet_addr "127.0.0.1"
        clientWInfo   <- initTorrentSession' port1 localhost info pid1
        checkMIPieceMgrInit clientWInfo
        clientWMagnet <- initMagnetSession' port2 localhost magnet pid2
        threadDelay 100000
        hsResult <- handshake clientWMagnet (SockAddrInet port1 localhost) hash
        -- hsResult <- handshake clientWInfo (SockAddrInet port2 localhost) hash
        threadDelay 1000000
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
      mi' <- readMVar mi
      assertBool "sessMIPieceMgr is not initialized" (isJust mi')

spawnTracker :: FilePath -> [String] -> IO ProcessHandle
spawnTracker pwd args = do
    (_, _, _, handle) <- createProcess (proc "opentracker" args){cwd=Just pwd}
    return handle
