{-# LANGUAGE RecordWildCards #-}
module Main where

import           Rho.Comms
import           Rho.Magnet
import           Rho.Metainfo
import           Rho.Tracker

import           Control.Concurrent
import qualified Data.ByteString.Char8 as B
import           Network.Socket
import           System.Environment    (getArgs)

main :: IO ()
main = do
    args <- getArgs
    -- contents <- B.readFile (head args)
    -- print $ parseMetainfo contents
    case (parseMagnet (B.pack $ head args)) of
      Left err -> putStrLn $ "can't parse magnet: " ++ err
      Right Magnet{..} ->
        case mTrackers of
          (UDPTracker addr_str port : _) -> do
            print port
            addrInfo <- getAddrInfo (Just defaultHints) (Just $ B.unpack addr_str) (Just $ show port)
            print addrInfo
            let trackerAddr = addrAddress (head addrInfo)
            putStrLn "initializing comm handler"
            sock <- initCommHandler
            putStrLn "comm handler initialized"
            sendConnectReq sock trackerAddr
            putStrLn "request sent, sleeping for 10 seconds"
            threadDelay 5000000
            return ()
          ts -> putStrLn $ "I don't like the trackers: " ++ show ts
