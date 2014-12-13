module Rho.ClientSpec where

import           Control.Concurrent
import qualified Data.ByteString          as B
import           Data.Either
import           Network.Socket
import           System.Directory
import           System.FilePath
import           System.Process

import           Test.Hspec
import           Test.Hspec.Contrib.HUnit
import           Test.HUnit

import           Rho.Metainfo
import           Rho.TrackerComms.UDP

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "client functions" $ do
      fromHUnitTest $ TestLabel "connecting" scrapeTest

scrapeTest :: Test
scrapeTest = TestCase $ do
    pwd <- getCurrentDirectory
    let torrentPath = pwd </> "test/test.torrent"
    torrentContents <- B.readFile torrentPath
    case parseMetainfo torrentContents of
      Left err -> assertFailure $ "Failed to parse torrent: " ++ err
      Right Metainfo{} -> do
        hostAddr <- inet_addr "127.0.0.1"
        let sockAddr = SockAddrInet (fromIntegral 6969) hostAddr

        putStrLn "Spawning tracker"
        tracker <- spawnTracker pwd []
        threadDelay 500000

        udpComms <- initUDPCommHandler
        ret <- connectRequest udpComms sockAddr
        assertBool "Peer can't connect" $ isRight ret
        terminateProcess tracker

spawnTracker :: FilePath -> [String] -> IO ProcessHandle
spawnTracker pwd args = do
    (_, _, _, handle) <- createProcess (proc "opentracker" args){cwd=Just pwd}
    return handle
