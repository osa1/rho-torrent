module Main where

import           Control.Applicative
import           Control.Monad
import qualified Data.ByteString.Builder   as BB
import qualified Data.ByteString.Char8     as B
import qualified Data.ByteString.Lazy      as LB
import           Data.Maybe
import           Data.Monoid
import           System.Environment        (getArgs)
import           System.Log.Formatter
import           System.Log.Handler
import           System.Log.Handler.Simple
import           System.Log.Logger
import           System.Random             (randomIO)

import           Rho.Magnet
import           Rho.Metainfo
import           Rho.PeerComms.Handshake
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
    case parseMagnet (B.pack magnetStr) of
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

-- | Generate 20-byte peer id.
generatePeerId :: IO PeerId
generatePeerId =
    PeerId . LB.toStrict . BB.toLazyByteString . mconcat . map BB.word8 <$> replicateM 20 randomIO

installLogger :: IO ()
installLogger = do
    lh <- fileHandler "logs.log" DEBUG
    let h = setFormatter lh (simpleLogFormatter "[$time : $loggername : $prio] $msg")
    updateGlobalLogger "Rho" (addHandler h . removeHandler)
