module Main where

import qualified Data.ByteString           as B
import qualified Data.ByteString.Char8     as BC
import           Data.Maybe
import           System.Environment        (getArgs)
import           System.Log.Formatter
import           System.Log.Handler
import           System.Log.Handler.Simple
import           System.Log.Logger

import           Rho.Magnet
import           Rho.Metainfo
import           Rho.PeerComms.PeerId
import           Rho.Session

main :: IO ()
main = do
    installLogger
    args <- getArgs
    case args of
      ["--magnet", magnetStr]    -> runMagnet magnetStr
      ["--torrent", torrentPath] -> runTorrent torrentPath
      _                          -> putStrLn $ "Don't know what to do with args: " ++ show args

runMagnet :: String -> IO ()
runMagnet magnetStr = do
    case parseMagnet (BC.pack magnetStr) of
      Left err -> error $ "Can't parse magnet string: " ++ err
      Right m@(Magnet _ trackers _) -> do
        pid  <- generatePeerId
        sess <- initMagnetSession m pid
        runMagnetSession sess trackers

runTorrent :: FilePath -> IO ()
runTorrent filePath = do
    contents <- B.readFile filePath
    case parseMetainfo contents of
      Left err -> putStrLn $ "Can't parse metainfo: " ++ err
      Right mi -> do
        pid <- generatePeerId
        sess <- initTorrentSession (mInfo mi) pid
        runTorrentSession sess (mAnnounce mi : concat (fromMaybe [] (mAnnounceList mi))) (mInfo mi)

installLogger :: IO ()
installLogger = do
    lh <- fileHandler "logs.log" DEBUG
    let h = setFormatter lh (simpleLogFormatter "[$time : $loggername : $prio] $msg")
    updateGlobalLogger "Rho" (addHandler h . removeHandler)
