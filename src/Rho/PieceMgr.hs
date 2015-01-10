{-# LANGUAGE MultiWayIf #-}

module Rho.PieceMgr where

import           Control.Applicative
import           Control.Concurrent.MVar
import           Control.Monad
import qualified Data.ByteString              as B
import qualified Data.ByteString.Char8        as BC
import           Data.Digest.SHA1             (hash)
import           Data.IORef
import           Data.List                    (foldl')
import           Data.Monoid
import qualified Data.Vector.Storable         as SV
import qualified Data.Vector.Storable.Mutable as MV
import           Data.Word
import           System.Directory             (doesFileExist)
import           System.FilePath              ((</>))

import qualified Rho.Bitfield                 as BF
import           Rho.Metainfo
import           Rho.Utils

data PieceMgr = PieceMgr
  { pmPieceSize :: Word32
  , pmTotalSize :: Word64
  , pmPieces    :: Word32
  , pmData      :: MVar (MV.IOVector Word8, BF.Bitfield, BF.Bitfield)
  }

newPieceMgr :: Word64 -> Word32 -> IO PieceMgr
newPieceMgr totalSize pieceLength = do
    arr  <- MV.new (fromIntegral totalSize)
    bits <- BF.empty (fromIntegral totalSize)
    let pieces =
          let (d, m) = totalSize `divMod` fromIntegral pieceLength
          in if m == 0 then d else d + 1
    pieceBits <- BF.empty (fromIntegral pieces)
    var  <- newMVar (arr, bits, pieceBits)
    return $ PieceMgr pieceLength totalSize (fromIntegral pieces) var

newPieceMgrFromData :: B.ByteString -> Word32 -> IO PieceMgr
newPieceMgrFromData bs pieceLength = do
    let totalSize = B.length bs
    arr  <- SV.unsafeThaw $ bsToByteVector $ B.copy bs
    bits <- BF.full totalSize
    let pieces =
          let (d, m) = totalSize `divMod` fromIntegral pieceLength
          in if m == 0 then d else d + 1
    pieceBits <- BF.full (fromIntegral pieces)
    var  <- newMVar (arr, bits, pieceBits)
    return $ PieceMgr pieceLength (fromIntegral totalSize) (fromIntegral pieces) var

pRange :: PieceMgr -> Word32 -> (Int, Int)
pRange (PieceMgr pSize pTotalSize pieces _) pIdx
  | pIdx >= pieces = error "requested range for a piece out of range"
  | otherwise      = let start = pSize * pIdx
                         end   = min (fromIntegral $ start + pSize) (fromIntegral pTotalSize)
                     in (fromIntegral $ start, end)

-- | Generate a ByteString from piece manager.
makeByteString :: PieceMgr -> IO B.ByteString
makeByteString (PieceMgr _ _ _ m) = BF.toBS . (\(_, _, bs) -> bs) =<< readMVar m

-- | Write piece data, return number of new(non-overwritten) bytes.
writePiece :: PieceMgr -> Word32 -> Word32 -> B.ByteString -> IO Int
writePiece pmgr@(PieceMgr pSize _ pieces m) pIdx pOffset pData = do
    let
      startIdx :: Int
      startIdx = fromIntegral pSize * fromIntegral pIdx + fromIntegral pOffset
      lastP :: Word32
      lastP    = fromIntegral ((startIdx + B.length pData) `div` fromIntegral pSize)

    news <- newIORef 0

    withMVar m $ \(arr, bits, pBits) -> do
      zipWithM_ (\byteIdx byte -> do
        MV.write arr  (startIdx + byteIdx) byte
        alreadySet <- BF.test bits (startIdx + byteIdx)
        unless alreadySet $ modifyIORef news (+ 1)
        BF.set   bits (startIdx + byteIdx)) [0..] (B.unpack pData)

      completedPieces <-
        filterM ((uncurry $ BF.checkRange bits) . pRange pmgr) [pIdx..(min lastP (pieces - 1))]

      forM_ completedPieces $ \ci -> BF.set pBits (fromIntegral ci)

    readIORef news

-- | Return piece data for given piece index, offest and length.
-- Length may be smaller then the given length for last piece.
-- Returns `Nothing` if either missing some part of the piece or given
-- piece is not in range.
getPieceData :: PieceMgr -> Word32 -> Word32 -> Word32 -> IO (Maybe B.ByteString)
getPieceData (PieceMgr pSize pTotal ps pData) pIdx pOffset pLen = do
    let
      start, end :: Int
      start = fromIntegral $ pIdx * pSize + pOffset
      end   = min (start + fromIntegral pLen) (fromIntegral pTotal)
    withMVar pData $ \(pd, bits, _) ->
      if | start >= end            -> return Nothing
         | fromIntegral pIdx >= ps -> return Nothing
         | otherwise -> do
             hasBytes <- BF.checkRange bits start end
             if hasBytes
               then Just . B.pack <$> (forM [start..end - 1] $ \idx -> MV.read pd idx)
               else return Nothing

getBytes :: PieceMgr -> IO B.ByteString
getBytes (PieceMgr _ _ _ pData) = do
    (d, _, _) <- readMVar pData
    bsFromByteVector <$> SV.freeze d

-- | Returns `Just (offset, length)` if we're missing some parts of the
-- given piece.
nextMissingPart :: PieceMgr -> Word32 -> IO (Maybe (Word32, Word32))
nextMissingPart pmgr@(PieceMgr _ _ _ m) pIdx = do
    (_, bits, pBits) <- readMVar m
    pieceComplete <- BF.test pBits (fromIntegral pIdx)
    if pieceComplete
      then return Nothing
      else do
        let (pStart, pEnd) = pRange pmgr pIdx
        ret <- findFirstMissing bits (fromIntegral pStart) (fromIntegral pEnd)
        case ret of
          Nothing           -> error "nextMissingPart returned nothing in wrong place"
          Just (start, end) -> do
            let offset = fromIntegral $ start - pStart
                len    = fromIntegral $ end - start
            return $ Just (offset, len)
  where
    findFirstMissing :: BF.Bitfield -> Int -> Int -> IO (Maybe (Int, Int))
    findFirstMissing v start end
      | start == end = return Nothing
      | otherwise    = do
          b <- BF.test v start
          if b then findFirstMissing v (start + 1) end
               else do
                 availIdx <- findFirstAvail v (start + 1) end
                 return $ Just (start, availIdx)

    findFirstAvail :: BF.Bitfield -> Int -> Int -> IO Int
    findFirstAvail v start end
      | start == end = return end
      | otherwise    = do
          b <- BF.test v start
          if not b then findFirstAvail v (start + 1) end
                   else return start

-- | Generate list of missing piece indexes.
missingPieces :: PieceMgr -> IO [Word32]
missingPieces (PieceMgr _ _ ps m) = do
    withMVar m $ \(_, _, pBits) ->
      filterM (\pIdx -> not <$> BF.test pBits (fromIntegral pIdx)) [0..ps-1]

-- | Check if piece data has correct hash.
checkPieces :: PieceMgr -> Word32 -> B.ByteString -> IO Bool
checkPieces (PieceMgr pSize totalSize pieces pData) pIdx pHash = do
    (arr, _, _) <- readMVar pData
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
    bytes <- SV.toList <$> SV.freeze (MV.slice (fromIntegral start) (fromIntegral end) arr)
    return $ word160ToBS (hash bytes) == pHash

-- | Generate files from given piece manager for the given torrent.
-- NOTE: This doesn't check hasehes.
generateFiles :: PieceMgr -> Info -> IO [(FilePath, B.ByteString)]
generateFiles (PieceMgr _ _ _ pData) (Info name _ _ _ _ files) = do
    let (names, sizes) = unzip $ map (\(p, s) -> (BC.unpack name </> p, s)) $ collectFiles files
    (bytes, _, _) <- readMVar pData
    bs <- splitBytes bytes sizes
    return $ zip names bs
  where
    splitBytes :: MV.IOVector Word8 -> [Word64] -> IO [B.ByteString]
    splitBytes _  []       = return []
    splitBytes ws (i : is) = do
      let (h, t) = MV.splitAt (fromIntegral i {- TODO: is this conversion safe? -}) ws
      rest <- splitBytes t is
      hl <- bsFromByteVector <$> SV.freeze h
      return (hl : rest)

    collectFiles :: Either File [File] -> [(FilePath, Word64)]
    collectFiles (Left f)   = [collectF f]
    collectFiles (Right fs) = map collectF fs

    collectF :: File -> (FilePath, Word64)
    collectF f = (mkPath $ fPath f, fLength f)

    mkPath :: [B.ByteString] -> String
    mkPath bs = foldl' (</>) "" $ map BC.unpack bs

-- | Try to generate a piece manager for the torrent from given root path.
--
-- If files exist in the path, generate pieces and check hashes. If hashes
-- match, create a new piece manager with pieces filled and return `True`.
-- Otherwise create an empty piece manager for the torrent and return
-- `False`.
tryReadFiles :: Info -> FilePath -> IO (PieceMgr, Bool)
tryReadFiles info root = do
    let root' = root </> BC.unpack (iName info)
        files = case iFiles info of
                  Left  _  -> [root']
                  Right fs -> map (\File{fPath=path} ->
                                      foldl' (</>) root' (map BC.unpack path)) fs
    allExist <- and <$> mapM doesFileExist files
    if allExist
      then do
        putStrLn "Files exist, checking hashes"
        bytes <- mconcat <$> mapM B.readFile files
        pieces <- newPieceMgrFromData bytes (iPieceLength info)
        hashesMatch <- and <$> zipWithM (checkPieces pieces) [0..] (iPieces info)
        if hashesMatch
          then do
            putStrLn "Hashes match, returning filled piece manager."
            return (pieces, True)
          else do
            putStrLn "Hashes don't match, returning empty piece manager."
            freshPieceManager
      else do
        putStrLn "Some files missing, returning empty piece manager."
        freshPieceManager
  where
    freshPieceManager :: IO (PieceMgr, Bool)
    freshPieceManager = do
      pieces <- newPieceMgr (torrentSize info) (iPieceLength info)
      return (pieces, False)
