{-# LANGUAGE RecordWildCards, ScopedTypeVariables #-}
module Main where

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Exception
import           Control.Monad
import qualified Data.ByteString.Builder       as BB
import qualified Data.ByteString.Char8         as B
import qualified Data.ByteString.Lazy          as LB
import           Data.Maybe
import           Data.Monoid
import           Data.Word
import           Network.Socket
import           System.Environment            (getArgs)
import           System.Log.Formatter
import           System.Log.Handler
import           System.Log.Handler.Simple
import           System.Log.Logger
import           System.Random                 (randomIO)

import           Rho.InfoHash
import           Rho.Magnet
import           Rho.Metainfo
import           Rho.PeerComms.Handshake
import           Rho.PeerComms.PeerConnection
import           Rho.PieceMgr
import           Rho.Session
import           Rho.SessionState
import           Rho.Torrent
import           Rho.Tracker
import           Rho.TrackerComms.HTTP
import           Rho.TrackerComms.PeerResponse
import           Rho.TrackerComms.UDP

main :: IO ()
main = do
    installLogger
    args <- getArgs
    case args of
      ["--magnet", magnetStr] -> runMagnet magnetStr
      ["--torrent", torrentPath] -> runTorrent torrentPath
      _ -> putStrLn $ "Don't know what to do with args: " ++ show args

requestPeers :: PeerId -> PortNumber -> InfoHash -> Torrent -> Tracker -> IO PeerResponse
requestPeers pid port hash torrent (HTTPTracker uri) = do
    putStrLn $ "HTTP: " ++ show uri
    peerRequestHTTP pid port uri torrent hash >>= either error return
requestPeers pid _ _ torrent udp@(UDPTracker host port) = do
    putStrLn $ "UDP: " ++ show udp
    print $ B.unpack host
    print port
    addrInfo <-
      (Just <$> getAddrInfo (Just defaultHints) (Just $ B.unpack host) (Just $ show port))
        `catch` \(_ :: IOException) -> return Nothing
    case addrInfo of
      Nothing -> return mempty
      Just addrInfo' -> do
        let trackerAddr = addrAddress (last addrInfo')
        commHandler <- initUDPCommHandler
        (peerRequestUDP commHandler trackerAddr pid torrent >>= either error return)
          `catch` \(_ :: IOException) -> return mempty

runMagnet :: String -> IO ()
runMagnet magnetStr = do
    case parseMagnet (B.pack magnetStr) of
      Left err -> error $ "Can't parse magnet string: " ++ err
      Right m@(Magnet hash trackers _) -> do
        peerId  <- generatePeerId
        let port = fromIntegral (5678 :: Word16)
        miDone <- newEmptyMVar
        torrentDone <- newEmptyMVar
        session <- initMagnetSession port m peerId (putMVar miDone ()) (putMVar torrentDone ())
        PeerResponse _ _ _ peers <- mconcat <$>
          mapM (requestPeers peerId port hash (mkTorrentFromMagnet m)) trackers
        forM_ peers $ \peer -> void $ forkIO $ void $ handshake session peer hash
        putStrLn $ "Waiting 5 seconds to establish connections with "
                   ++ show (length peers) ++ " peers."
        threadDelay (1000000 * 5)
        putStrLn $ "Sending metainfo requests."
        miPieceMgr <- fromJust <$> readMVar (sessMIPieceMgr session)

        loopThread <- async $ loop session miPieceMgr
        miDoneThread <- async $ void $ readMVar miDone

        -- loop thread never terminates, I'm just using `waitAnyCancel` to
        -- interrupt loop thread when metainfo download is complete.
        void $ waitAnyCancel [loopThread, miDoneThread]

        putStrLn $ "Downloaded the info. Parsing..."
        bytes <- getBytes miPieceMgr
        case parseInfoDict bytes of
          Left err   -> error $ "Can't parse info dict: " ++ err
          Right info -> do
            print info
            if iHash info == hash
              then putStrLn "Hash correct"
              else putStrLn "Wrong hash"
  where
    loop sess pieces = do
      sendMetainfoRequests (sessPeers sess) pieces
      threadDelay (1000000 * 5)
      loop sess pieces

runTorrent :: FilePath -> IO ()
runTorrent filePath = do
    contents <- B.readFile filePath
    case parseMetainfo contents of
      Left err -> putStrLn $ "Can't parse metainfo: " ++ err
      Right mi -> do
        let hash = iHash $ mInfo mi
        peerId <- generatePeerId
        let port = fromIntegral (5678 :: Word16)
        torrentDone <- newEmptyMVar
        session <- initTorrentSession port (mInfo mi) peerId (putMVar torrentDone ())
        PeerResponse _ _ _ peers <-
          requestPeers peerId port hash (mkTorrentFromMetainfo mi) (mAnnounce mi)
        forM_ peers $ \peer -> void $ forkIO $ void $ handshake session peer hash
        putStrLn $ "Waiting 5 seconds to establish connections with "
                   ++ show (length peers) ++ " peers."
        threadDelay (1000000 * 5)
        runPeers session torrentDone

runPeers :: Session -> MVar () -> IO ()
runPeers sess torrentDone = do
    pieces <- fromJust <$> readMVar (sessPieceMgr sess)
    loopThread <- async $ loop pieces
    torrentDoneThread <- async $ readMVar torrentDone

    void $ waitAnyCancel [loopThread, torrentDoneThread]
    putStrLn "Torrent download completed."
  where
    loop :: PieceMgr -> IO ()
    loop pieces = do
      sendPieceRequests (sessPeers sess) pieces
      threadDelay 1000000
      loop pieces

-- | Generate 20-byte peer id.
generatePeerId :: IO PeerId
generatePeerId =
    PeerId . LB.toStrict . BB.toLazyByteString . mconcat . map BB.word8 <$> replicateM 20 randomIO

installLogger :: IO ()
installLogger = do
    lh <- fileHandler "logs.log" DEBUG
    let h = setFormatter lh (simpleLogFormatter "[$time : $loggername : $prio] $msg")
    updateGlobalLogger "Rho" (addHandler h . removeHandler)
