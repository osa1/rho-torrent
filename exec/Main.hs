{-# LANGUAGE RecordWildCards #-}
module Main where

import           Rho.Comms
import           Rho.Magnet
import           Rho.Metainfo
import           Rho.Torrent
import           Rho.Tracker

import           Control.Applicative
import           Control.Concurrent
import           Control.Monad
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8   as B
import qualified Data.ByteString.Lazy    as LB
import           Data.Monoid
import           Data.Word
import           Network.Socket
import           System.Environment      (getArgs)
import           System.Random           (randomIO)

main :: IO ()
main = do
    args <- getArgs
    -- contents <- B.readFile (head args)
    -- print $ parseMetainfo contents
    case (parseMagnet (B.pack $ head args)) of
      Left err -> putStrLn $ "can't parse magnet: " ++ err
      Right m@Magnet{..} ->
        case mTrackers of
          (UDPTracker addr_str port : _) -> do
            peerId <- generatePeerId
            print port
            addrInfo <- getAddrInfo (Just defaultHints) (Just $ B.unpack addr_str) (Just $ show port)
            print addrInfo
            let trackerAddr = addrAddress (head addrInfo)
            putStrLn "initializing comm handler"
            (sock, cbs) <- initCommHandler
            putStrLn "comm handler initialized"
            tid <- sendPeersReq sock trackerAddr peerId (mkTorrentFromMagnet m) cbs
            putStrLn $ "request sent, sleeping for 10 seconds (tid: " ++ show tid ++ ")"
            threadDelay 5000000
            return ()
          ts -> putStrLn $ "I don't like the trackers: " ++ show ts

-- | Generate 20-byte peer id.
--
-- I don't need how this is really used. All I can see is that we send
-- this to tracker in annonuce request but why not generate a fresh
-- peer id for every request? Looked pretty useless to me.
generatePeerId :: IO B.ByteString
generatePeerId =
    LB.toStrict . BB.toLazyByteString . mconcat . map BB.word8 <$> replicateM 20 randomIO
