module Rho.PieceMgr where

import           Control.Concurrent.MVar
import           Control.Monad
import qualified Data.Array.IO           as A
import qualified Data.ByteString         as B
import           Data.Word

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
