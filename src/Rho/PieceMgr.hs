module Rho.PieceMgr where

import           Control.Applicative
import           Control.Concurrent.MVar
import           Control.Monad
import qualified Data.ByteString             as B
import qualified Data.ByteString.Char8       as BC
import           Data.Digest.SHA1            (hash)
import           Data.List                   (foldl')
import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as MV
import           Data.Word
import           System.FilePath             ((</>))

import           Rho.Metainfo
import           Rho.Utils

data PieceMgr = PieceMgr
  { pmPieceSize :: Word32
  , pmTotalSize :: Word64
  , pmPieces    :: Word32
  , pmData      :: MVar ( MV.IOVector Word8
                        , MV.IOVector Bool )
  }

-- | (piece index, offset in piece, length until next piece) triple.
type PieceData = (Word32, Word32, Word32)

newPieceMgr :: Word64 -> Word32 -> IO PieceMgr
newPieceMgr totalSize pieceLength = do
    arr <- MV.new (fromIntegral totalSize)
    bits <- MV.replicate (fromIntegral totalSize) False
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
      MV.write arr  (arrIdx + byteIdx) byte
      MV.write bits (arrIdx + byteIdx) True)
      [0..] (B.unpack pieceData)
    putMVar m (arr, bits)

-- | Generate list of (piece index, piece offset, length until next piece)
-- triples.
--
-- TODO: Benchmark this. Use caching if it turns out to be too slow.
-- (Cache the result. Invalidate the cache when a new piece is written)
missingPieces :: PieceMgr -> IO [PieceData]
missingPieces (PieceMgr ps ts totalPieces m) = do
    (arr, bits) <- takeMVar m
    ret <- collectMissingPieces arr bits (ts - 1) 0
    putMVar m (arr, bits)
    return ret
  where
    collectMissingPieces
      :: MV.IOVector Word8 -> MV.IOVector Bool -> Word64 -> Word64
      -> IO [(Word32, Word32, Word32)]
    collectMissingPieces arr bits end idx
      | idx > end = return []
      | otherwise = do
          bit <- MV.read bits (fromIntegral idx)
          if bit
            then collectMissingPieces arr bits end (idx + 1)
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
                  rest <- collectMissingPieces arr bits end (idx + fromIntegral offsetToNextPiece)
                  return $ (currentPiece, pieceOffset, offsetToNextPiece) : rest

-- | Check if piece data has correct hash.
checkPieces :: PieceMgr -> Word32 -> B.ByteString -> IO Bool
checkPieces (PieceMgr pSize totalSize pieces pData) pIdx pHash = do
    (arr, _) <- readMVar pData
    let
      isLastPiece = pIdx == pieces - 1
      start :: Word64
      start = fromIntegral pIdx * fromIntegral pSize
      end :: Word64
      end = if isLastPiece
              then start + (if totalSize `mod` fromIntegral pSize == 0
                              then fromIntegral pSize
                              else totalSize `mod` fromIntegral pSize)
              else start + fromIntegral pSize
    bytes <- V.toList <$> V.freeze (MV.slice (fromIntegral start) (fromIntegral end) arr)
    return $ word160ToBS (hash bytes) == pHash

generateFiles :: PieceMgr -> Info -> IO [(FilePath, B.ByteString)]
generateFiles (PieceMgr _ _ _ pData) (Info name _ _ _ files) = do
    let (names, sizes) = unzip $ map (\(p, s) -> (BC.unpack name </> p, s)) $ collectFiles files
    bytes <- fst <$> readMVar pData
    unless (fromIntegral (sum sizes) == MV.length bytes) . error . concat $
      [ "sum sizes /= length bytes ("
      , show (sum sizes), " /= ", show (MV.length bytes), ")" ]
    bs <- splitBytes bytes sizes
    return $ zip names $ map B.pack bs
  where
    splitBytes :: MV.IOVector Word8 -> [Word64] -> IO [[Word8]]
    splitBytes _  []       = return []
    splitBytes ws (i : is) = do
      let (h, t) = MV.splitAt (fromIntegral i {- TODO: is this conversion safe? -}) ws
      rest <- splitBytes t is
      hl <- V.toList <$> V.freeze h
      return (hl : rest)

    collectFiles :: Either File [File] -> [(FilePath, Word64)]
    collectFiles (Left f)   = [collectF f]
    collectFiles (Right fs) = map collectF fs

    collectF :: File -> (FilePath, Word64)
    collectF f = (mkPath $ fPath f, fLength f)

    mkPath :: [B.ByteString] -> String
    mkPath bs = foldl' (</>) "" $ map BC.unpack bs
