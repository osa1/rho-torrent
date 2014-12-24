{-# LANGUAGE RecordWildCards, ScopedTypeVariables #-}
module Main where

import           Rho.InfoHash
import           Rho.Magnet
import           Rho.Metainfo
import           Rho.PeerComms.Handshake
import           Rho.PeerComms.Message
import           Rho.PeerComms.PeerConnection
import           Rho.PieceMgr
import           Rho.Session
import           Rho.SessionState
import           Rho.Torrent
import           Rho.Tracker
import           Rho.TrackerComms.HTTP
import           Rho.TrackerComms.PeerResponse
import           Rho.TrackerComms.UDP

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Exception
import           Control.Monad
import qualified Data.ByteString.Builder       as BB
import qualified Data.ByteString.Char8         as B
import qualified Data.ByteString.Lazy          as LB
import           Data.IORef
import qualified Data.Map                      as M
import           Data.Maybe
import           Data.Monoid
import           Network.Socket
import           System.Environment            (getArgs)
import           System.Log.Formatter
import           System.Log.Handler
import           System.Log.Handler.Simple
import           System.Log.Logger
import           System.Random                 (randomIO)

main :: IO ()
main = do
    installLogger
    args <- getArgs
    case args of
      ["--magnet", magnetStr] -> runMagnet magnetStr
      ["--torrent", torrentPath] -> runTorrent torrentPath
      ["--magnet", magnetStr, "--scrape"] -> scrapeMagnet magnetStr
      _ -> putStrLn $ "Don't know what to do with args: " ++ show args

scrapeMagnet :: String -> IO ()
scrapeMagnet = undefined

requestPeers :: PeerId -> InfoHash -> Torrent -> Tracker -> IO PeerResponse
requestPeers pid hash torrent (HTTPTracker uri) = do
    putStrLn $ "HTTP: " ++ show uri
    peerRequestHTTP pid uri torrent hash >>= either error return
requestPeers pid _ torrent udp@(UDPTracker host port) = do
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
        PeerResponse _ _ _ peers <- mconcat <$>
          mapM (requestPeers peerId hash (mkTorrentFromMagnet m)) trackers
        session <- initMagnetSession 5678 m peerId
        forM_ peers $ \peer -> void $ forkIO $ void $ handshake session peer hash
        putStrLn $ "Waiting 5 seconds to establish connections with "
                   ++ show (length peers) ++ " peers."
        threadDelay (1000000 * 5)
        putStrLn $ "Sending metainfo requests."
        miPieceMgr <- fromJust <$> readMVar (sessMIPieceMgr session)
        loop session miPieceMgr
        putStrLn $ "Downloaded the info. Parsing..."
        bytes <- getBytes miPieceMgr
        case parseInfoDict bytes of
          Left err   -> error $ "Can't parse info dict: " ++ err
          Right info -> do
            print info
            if iHash info == hash
              then putStrLn "Hash correct"
              else putStrLn "Wrong hash"
        return ()
  where
    loop sess pieces = do
      missings <- missingPieces pieces
      unless (null missings) $ do
        sendMetainfoRequests (sessPeers sess) pieces
        threadDelay (1000000 * 5)
        loop sess pieces

runTorrent :: FilePath -> IO ()
runTorrent filePath = do
    contents <- B.readFile filePath
    case parseMetainfo contents of
      Left err -> putStrLn $ "Can't parse metainfo: " ++ err
      Right m@Metainfo{mAnnounce=HTTPTracker uri} -> do
        putStrLn $ "info_hash: " ++ show (iHash $ mInfo m)
        peerId <- generatePeerId
        req <- peerRequestHTTP peerId uri (mkTorrentFromMetainfo m) (iHash $ mInfo m)
        runPeers req (mInfo m) (iHash $ mInfo m) peerId
      Right m@Metainfo{mAnnounce=UDPTracker addr_str port} -> do
        peerId <- generatePeerId
        addrInfo <- getAddrInfo (Just defaultHints) (Just $ B.unpack addr_str) (Just $ show port)
        let trackerAddr = addrAddress (last addrInfo)
        putStrLn "initializing comm handler"
        commHandler <- initUDPCommHandler
        putStrLn "comm handler initialized"
        peers <- peerRequestUDP commHandler trackerAddr peerId (mkTorrentFromMetainfo m)
        runPeers peers (mInfo m) (iHash $ mInfo m) peerId

-- runPeers = undefined

runPeers :: Either String PeerResponse -> Info -> InfoHash -> PeerId -> IO ()
runPeers (Left err) _ _ _ = error err
runPeers (Right peers) info hash peerId = do
    putStrLn $ "Sending handshake to peers..."
    sess <- initTorrentSession (fromIntegral (5433 :: Int)) info peerId
    forM_ (prPeers peers) $ \peer -> do
      async $ handshake sess peer hash

    -- threadDelay 30000000
    connectedPeers <- M.elems `fmap` readMVar (sessPeers sess)
    putStrLn $ "Peers: " ++ show (length connectedPeers)

    threadDelay 1000000

    ps <- M.toList `fmap` readMVar (sessPeers sess)
    forM_ ps $ \(addr, peerConn) -> do
      pc <- readIORef peerConn
      putStrLn $ "Sending interested to: " ++ show addr
      void $ sendMessage pc Unchoke
      void $ sendMessage pc Interested

    threadDelay 100000

    void $ forever $ do
      putStrLn "Sending piece requests"
      pm <- readMVar $ sessPieceMgr sess
      sendPieceRequests (sessPeers sess) (fromJust pm)
      threadDelay 10000000

    connectedPeers' <- M.elems `fmap` readMVar (sessPeers sess)
    putStrLn $ "Peers: " ++ show (length connectedPeers')

-- | Generate 20-byte peer id.
--
-- I don't need how this is really used. All I can see is that we send
-- this to trackers in annonuce request but why not generate a fresh
-- peer id for every request? Looked pretty useless to me.
generatePeerId :: IO PeerId
generatePeerId =
    PeerId . LB.toStrict . BB.toLazyByteString . mconcat . map BB.word8 <$> replicateM 20 randomIO

installLogger :: IO ()
installLogger = do
    lh <- fileHandler "logs.log" DEBUG
    let h = setFormatter lh (simpleLogFormatter "[$time : $loggername : $prio] $msg")
    updateGlobalLogger "Rho" (addHandler h . removeHandler)
