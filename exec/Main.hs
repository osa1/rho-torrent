{-# LANGUAGE RecordWildCards #-}
module Main where

import           Rho.Comms
import           Rho.Magnet
import           Rho.Metainfo
import           Rho.PeerComms
import           Rho.Torrent
import           Rho.Tracker

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad
import qualified Data.ByteString.Builder  as BB
import qualified Data.ByteString.Char8    as B
import qualified Data.ByteString.Lazy     as LB
import qualified Data.Map                 as M
import           Data.Monoid
import           Data.Word
import           Network.Socket
import           System.Environment       (getArgs)
import           System.Random            (randomIO)

main :: IO ()
main = do
    args <- getArgs
    case args of
      ["--magnet", magnetStr] -> runMagnet magnetStr
      ["--torrent", torrentPath] -> runTorrent torrentPath
      _ -> putStrLn $ "Don't know what to do with args: " ++ show args

-- This looks like working, but I never get any useful responses from
-- servers. They may be problems regarding info_hash generation.
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
            (sock, cbs, peerRps) <- initCommHandler
            putStrLn "comm handler initialized"
            tid <- sendPeersReq sock trackerAddr peerId (mkTorrentFromMagnet m) cbs
            putStrLn $ "request sent, sleeping for 5 seconds (tid: " ++ show tid ++ ")"
            threadDelay 5000000
            putStrLn $ "Sending handshake to peers..."
            peerRps' <- readMVar peerRps
            peerStatus <- newMVar M.empty
            case M.lookup tid peerRps' of
              Nothing -> putStrLn "can't find peer response"
              Just PeerResponse{prPeers=peers} ->
                forM_ peers $ \peer -> do
                  async $ handshake peer mHash peerId peerStatus

            threadDelay 50000000

            return ()
          ts -> putStrLn $ "I don't like the trackers: " ++ show ts

runTorrent :: FilePath -> IO ()
runTorrent filePath = do
    contents <- B.readFile filePath
    case parseMetainfo contents of
      Left err -> putStrLn $ "can't parse metainfo: " ++ err
      Right m@Metainfo{mAnnounce=HTTPTracker uri} -> do
        putStrLn $ "info_hash: " ++ show (iHash $ mInfo m)
        peerId <- generatePeerId
        req <- sendGetRequest peerId uri (mkTorrentFromMetainfo m) m
        resp <- wait req
        print resp

-- | Generate 20-byte peer id.
--
-- I don't need how this is really used. All I can see is that we send
-- this to trackers in annonuce request but why not generate a fresh
-- peer id for every request? Looked pretty useless to me.
generatePeerId :: IO B.ByteString
generatePeerId =
    LB.toStrict . BB.toLazyByteString . mconcat . map BB.word8 <$> replicateM 20 randomIO
