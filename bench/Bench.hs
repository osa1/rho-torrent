{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Main where

import           Control.Applicative
import           Control.Concurrent
import           Control.DeepSeq
import           Criterion.Main
import qualified Data.ByteString              as B
import qualified Data.Vector.Storable.Mutable as MV
import           Data.Word
import           System.Directory
import           System.FilePath

import qualified Rho.Bitfield                 as BF
import           Rho.Metainfo
import           Rho.PieceMgr
import           Rho.Tracker

main :: IO ()
main = defaultMain
  [ env loadFiles $ \files -> bgroup ("decoding files using `bencoding` library")
    [ bench "decode" $ nf (map parseMetainfo) files
    ]
  , env (generatePieceMgr (1000 * 1000) 1024) $ \pMgr ->
      bench "generating piece hashes" $ nfIO (mapM (generatePieceHash pMgr) [0..99])
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

generatePieceMgr :: Word64 -> Word32 -> IO PieceMgr
generatePieceMgr totalSize pieceSize = PieceMgr pieceSize totalSize pieces <$> pmDataGen
  where
    pieces :: Word32
    pieces = fromIntegral $ (totalSize `div` fromIntegral pieceSize)

    pmDataGen :: IO (MVar (MV.IOVector Word8, BF.Bitfield, BF.Bitfield))
    pmDataGen = do
      vec <- MV.new (fromIntegral totalSize)
      fillVec (fromIntegral totalSize) vec
      bf1 <- BF.full (fromIntegral totalSize)
      bf2 <- BF.full (fromIntegral pieces)
      newMVar (vec, bf1, bf2)

    fillVec :: Int -> MV.IOVector Word8 -> IO ()
    fillVec size vec = loop 0 fileContent
      where
        loop vIdx [] = loop vIdx fileContent
        loop vIdx (c : cs)
          | vIdx == size = return ()
          | otherwise    = do
              MV.unsafeWrite vec vIdx c
              loop (vIdx + 1) cs

    fileContent :: [Word8]
    fileContent = B.unpack "dummy data "

instance NFData Tracker where
    rnf (HTTPTracker _) = ()
    rnf (UDPTracker bs _) = rnf bs

-- TODO: Is there a way to deepseq references?
instance NFData PieceMgr where
    rnf (PieceMgr w1 w2 w3 _) = rnf w1 `seq` rnf w2 `seq` rnf w3

instance NFData BF.Bitfield where
    rnf (BF.Bitfield v i) = rnf v `seq` rnf i
