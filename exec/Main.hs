{-# LANGUAGE RecordWildCards #-}
module Main where

import           Rho.InfoHash
import           Rho.Magnet
import           Rho.Metainfo
import           Rho.PeerComms
import           Rho.PeerComms.Handshake
import           Rho.PeerComms.Message
import           Rho.Torrent
import           Rho.Tracker
import           Rho.TrackerComms.HTTP
import           Rho.TrackerComms.PeerResponse
import           Rho.TrackerComms.UDP

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad
import qualified Data.ByteString.Builder       as BB
import qualified Data.ByteString.Char8         as B
import qualified Data.ByteString.Lazy          as LB
import qualified Data.Map                      as M
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
scrapeMagnet magnetStr = do
    case (parseMagnet (B.pack magnetStr)) of
      Left err -> putStrLn $ "can't parse magnet: " ++ err
      Right m@Magnet{..} ->
        case mTrackers of
          (UDPTracker addr_str port : _) -> do
            peerId <- generatePeerId
            addrInfo <- getAddrInfo (Just defaultHints) (Just $ B.unpack addr_str) (Just $ show port)
            let trackerAddr = addrAddress (last addrInfo)
            putStrLn "initializing comm handler"
            commHandler <- initUDPCommHandler
            putStrLn "comm handler initialized"
            peers <- scrapeRequestUDP commHandler trackerAddr [mHash]
            print peers

runMagnet :: String -> IO ()
runMagnet magnetStr = do
    case (parseMagnet (B.pack magnetStr)) of
      Left err -> putStrLn $ "can't parse magnet: " ++ err
      Right m@Magnet{..} ->
        case mTrackers of
          (UDPTracker addr_str port : _) -> do
            peerId <- generatePeerId
            addrInfo <- getAddrInfo (Just defaultHints) (Just $ B.unpack addr_str) (Just $ show port)
            let trackerAddr = addrAddress (last addrInfo)
            putStrLn "initializing comm handler"
            commHandler <- initUDPCommHandler
            putStrLn "comm handler initialized"
            peers <- peerRequestUDP commHandler trackerAddr peerId (mkTorrentFromMagnet m)
            runPeers peers mHash peerId
          ts -> putStrLn $ "I don't like the trackers: " ++ show ts

runTorrent :: FilePath -> IO ()
runTorrent filePath = do
    contents <- B.readFile filePath
    case parseMetainfo contents of
      Left err -> putStrLn $ "can't parse metainfo: " ++ err
      Right m@Metainfo{mAnnounce=HTTPTracker uri} -> do
        putStrLn $ "info_hash: " ++ show (iHash $ mInfo m)
        peerId <- generatePeerId
        req <- peerRequestHTTP peerId uri (mkTorrentFromMetainfo m) m
        case req of
          Left err -> putStrLn $ "Error happened: " ++ err
          Right peerResp -> do
            peerComms <- initPeerCommsHandler
            forM_ (prPeers peerResp) $ \peer -> do
              async $ handshake peerComms peer (iHash $ mInfo m) peerId
            threadDelay 30000000
            putStr "Peers: "
            putStrLn . show . M.size =<< readMVar (pchPeers peerComms)
      Right m@Metainfo{mAnnounce=UDPTracker addr_str port} -> do
        peerId <- generatePeerId
        addrInfo <- getAddrInfo (Just defaultHints) (Just $ B.unpack addr_str) (Just $ show port)
        let trackerAddr = addrAddress (last addrInfo)
        putStrLn "initializing comm handler"
        commHandler <- initUDPCommHandler
        putStrLn "comm handler initialized"
        peers <- peerRequestUDP commHandler trackerAddr peerId (mkTorrentFromMetainfo m)
        runPeers peers (iHash $ mInfo m) peerId

runPeers :: Either String PeerResponse -> InfoHash -> PeerId -> IO ()
runPeers (Left err) _ _ = error err
runPeers (Right peers) infoHash peerId = do
    putStrLn $ "Sending handshake to peers..."
    peerComms <- initPeerCommsHandler
    forM_ (prPeers peers) $ \peer -> do
      async $ handshake peerComms peer infoHash peerId
    threadDelay 30000000
    connectedPeers <- M.elems `fmap` readMVar (pchPeers peerComms)
    putStrLn $ "Peers: " ++ show (length connectedPeers)
    ps <- M.toList `fmap` readMVar (pchPeers peerComms)
    forM_ ps $ \(addr, peerConn) -> do
      putStrLn $ "Sending extended handshake to: " ++ show addr
      sendMessage peerConn (Extended (ExtendedHandshake defaultMsgTable []))
    threadDelay 30000000
    connectedPeers' <- M.elems `fmap` readMVar (pchPeers peerComms)
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
