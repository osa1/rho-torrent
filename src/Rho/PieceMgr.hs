module Rho.PieceMgr where

import           Control.Applicative
import           Control.Concurrent.MVar
import           Control.Monad
import qualified Data.Array.IO           as A
import qualified Data.ByteString         as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8   as BC
import qualified Data.ByteString.Lazy    as LB
import           Data.Digest.SHA1        (Word160 (..), hash)
import           Data.List               (foldl')
import           Data.Monoid
import           Data.Word
import           System.FilePath         ((</>))

import           Rho.Metainfo

data PieceMgr = PieceMgr
  { pmPieceSize :: Word32
  , pmTotalSize :: Word64
  , pmPieces    :: Word32
  , pmData      :: MVar ( A.IOUArray Word64 Word8
                        , A.IOUArray Word64 Bool )
  }

newPieceMgr :: Word64 -> Word32 -> IO PieceMgr
newPieceMgr totalSize pieceLength = do
    arr <- A.newArray (0, totalSize - 1) 0
    bits <- A.newArray (0, totalSize - 1) False
    var <- newMVar (arr, bits)
    let pieces =
          let (d, m) = totalSize `divMod` fromIntegral pieceLength
          in if m == 0 then d else d + 1
    return $ PieceMgr pieceLength totalSize (fromIntegral pieces) var

writePiece :: PieceMgr -> Word32 -> Word32 -> B.ByteString -> IO ()
writePiece (PieceMgr ps _ _ m) pieceIdx pieceOffset pieceData = do
    (arr, bits) <- takeMVar m
    zipWithM_ (\byteIdx byte -> do
      let arrIdx = fromIntegral ps * fromIntegral pieceIdx + fromIntegral pieceOffset
      A.writeArray arr  (arrIdx + byteIdx) byte
      A.writeArray bits (arrIdx + byteIdx) True)
      [0..] (B.unpack pieceData)
    putMVar m (arr, bits)

missingPieces :: PieceMgr -> IO [(Word32, Word32, Word32)]
missingPieces (PieceMgr ps ts totalPieces m) = do
    (arr, bits) <- takeMVar m
    ret <- collectMissingPieces arr bits (ts - 1) 0
    putMVar m (arr, bits)
    return ret
  where
    collectMissingPieces
      :: A.IOUArray Word64 Word8 -> A.IOUArray Word64 Bool -> Word64 -> Word64
      -> IO [(Word32, Word32, Word32)]
    collectMissingPieces arr bits max idx
      | idx > max = return []
      | otherwise = do
          bit <- A.readArray bits idx
          if bit
            then collectMissingPieces arr bits max (idx + 1)
            else do
              let offsetToNextPiece, currentPiece, pieceOffset :: Word32
                  offsetToNextPiece = fromIntegral $ fromIntegral ps - (idx `mod` fromIntegral ps)
                  currentPiece      = fromIntegral $ idx `div` fromIntegral ps
                  pieceOffset       = fromIntegral $ ps - offsetToNextPiece
              if currentPiece == totalPieces - 1
                then do
                  -- last piece
                  let md = ts `mod` fromIntegral ps
                  return [(currentPiece, 0, if md == 0 then ps else fromIntegral md)]
                else do
                  rest <- collectMissingPieces arr bits max (idx + fromIntegral offsetToNextPiece)
                  return $ (currentPiece, pieceOffset, offsetToNextPiece) : rest

-- | Check if piece data has correct hash.
checkPieces :: PieceMgr -> Word32 -> B.ByteString -> IO Bool
checkPieces (PieceMgr pSize totalSize pieces pData) pIdx pHash = do
    (arr, _) <- readMVar pData
    let isLastPiece = pIdx == pieces - 1
        start :: Word64
        start = fromIntegral $ pIdx * pSize
    bytes <- slice arr start $
               if isLastPiece
                 then start + (if totalSize `mod` fromIntegral pSize == 0
                                 then fromIntegral pSize
                                 else totalSize `mod` fromIntegral pSize)
                 else start + fromIntegral pSize
    return $ word160ToBS (hash bytes) == pHash

generateFiles :: PieceMgr -> Info -> IO [(FilePath, B.ByteString)]
generateFiles (PieceMgr _ _ _ pData) (Info name _ _ _ files) = do
    let fs = map (\(p, s) -> (BC.unpack name </> p, s)) $ collectFiles files
    bytes <- readMVar pData >>= A.getElems . fst
    return $ zip (map fst fs) $ map B.pack $ splitBytes bytes (map snd fs)
  where
    splitBytes :: [Word8] -> [Int] -> [[Word8]]
    splitBytes [] [] = []
    splitBytes ws (i : is) =
      let (h, t) = splitAt i ws
          rest   = splitBytes t is
      in (h : rest)

    collectFiles :: Either File [File] -> [(FilePath, Int)]
    collectFiles (Left f)   = [collectF f]
    collectFiles (Right fs) = map collectF fs

    collectF :: File -> (FilePath, Int)
    collectF f = (mkPath $ fPath f, fLength f)

    mkPath :: [B.ByteString] -> String
    mkPath bs = foldl' (</>) "" $ map BC.unpack bs

-- * Utils

-- FIXME: This may be too slow because of redundant arr -> list conversion.
-- FIXME: Maybe move to utils.
-- FIXME: Write tests before improving the performance.
slice :: A.IOUArray Word64 Word8 -> Word64 -> Word64 -> IO [Word8]
slice arr start end =
    take (fromIntegral $ end - start) . drop (fromIntegral start) <$> A.getElems arr

word160ToBS :: Word160 -> B.ByteString
word160ToBS (Word160 w1 w2 w3 w4 w5) = LB.toStrict . BB.toLazyByteString . mconcat $
    map BB.word32BE [w1, w2, w3, w4, w5]
