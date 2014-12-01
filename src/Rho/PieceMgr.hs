module Rho.PieceMgr where

import           Control.Concurrent.MVar
import           Control.Monad
import qualified Data.Array.IO           as A
import qualified Data.ByteString         as B
import           Data.Word

data PieceMgr = PieceMgr
  { pmPieceSize :: Word32
  , pmData      :: MVar ( A.IOUArray Word64 Word8
                        , A.IOUArray Word64 Bool )
  }

newPieceMgr :: Word64 -> Word32 -> IO PieceMgr
newPieceMgr size pieceSize = do
    arr <- A.newArray (0, size - 1) 0
    bits <- A.newArray (0, size - 1) False
    var <- newMVar (arr, bits)
    return $ PieceMgr pieceSize var

writePiece :: PieceMgr -> Word32 -> Word32 -> B.ByteString -> IO ()
writePiece (PieceMgr ps m) pieceIdx pieceOffset pieceData = do
    (arr, bits) <- takeMVar m
    zipWithM_ (\byteIdx byte -> do
      let arrIdx = fromIntegral ps * fromIntegral pieceIdx + fromIntegral pieceOffset
      A.writeArray arr  (arrIdx + byteIdx) byte
      A.writeArray bits (arrIdx + byteIdx) True)
      [0..] (B.unpack pieceData)
    putMVar m (arr, bits)

missingPieces :: PieceMgr -> IO [(Word32, Word32)]
missingPieces (PieceMgr ps m) = do
    (arr, bits) <- takeMVar m
    (_, max) <- A.getBounds arr
    ret <- collectMissingPieces arr bits max 0
    putMVar m (arr, bits)
    return ret
  where
    collectMissingPieces
      :: A.IOUArray Word64 Word8 -> A.IOUArray Word64 Bool -> Word64 -> Word64 -> IO [(Word32, Word32)]
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
              rest <- collectMissingPieces arr bits max (idx + fromIntegral offsetToNextPiece)
              return $ (currentPiece, pieceOffset) : rest
