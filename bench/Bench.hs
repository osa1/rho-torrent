{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           Control.DeepSeq
import           Criterion.Main
import qualified Data.ByteString  as B
import           Rho.Metainfo
import           System.Directory
import           System.FilePath

import           Rho.InfoHash
import           Rho.Tracker

main :: IO ()
main = defaultMain
  [ env loadFiles $ \files -> bgroup ("decoding files using `bencoding` library")
    [ bench "decode" $ nf (\() -> force $ map parseMetainfo files) ()
    ]
  ]

root :: FilePath
root = "tests/should_parse/ubuntu/torrent.ubuntu.com:6969"

loadFiles :: IO [B.ByteString]
loadFiles = do
    rootExists <- doesDirectoryExist root
    if rootExists
      then do
        contents <- filter (\f -> head f /= '.') `fmap` getDirectoryContents root
        files <- mapM (B.readFile . (root </>)) contents
        putStrLn $ "Returning " ++ show (length files) ++ " files."
        return $ force files
      else do
        putStrLn "Test files are missing. Won't run benchmarks."
        return []

instance NFData Metainfo where
    rnf Metainfo{..} = rnf mAnnounce `seq` rnf mAnnounceList `seq` rnf mCreationDate `seq`
                       rnf mComment `seq` rnf mCreatedBy `seq` rnf mEncoding `seq` rnf mInfo

instance NFData Info where
    rnf Info{..} = rnf iName `seq` rnf iPieceLength `seq` rnf iPieces `seq` rnf iPrivate `seq`
                   rnf iFiles `seq` rnf iHash

instance NFData File where
    rnf File{..} = rnf fLength `seq` rnf fMd5Sum `seq` rnf fPath

instance NFData InfoHash where
    rnf (InfoHash hash) = rnf hash

instance NFData Tracker where
    rnf (HTTPTracker _) = ()
    rnf (UDPTracker bs _) = rnf bs
