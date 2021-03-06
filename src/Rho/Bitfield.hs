module Rho.Bitfield where

import           Control.Monad
import qualified Data.Bits                    as B
import qualified Data.ByteString              as B
import qualified Data.Set                     as S
import qualified Data.Vector.Storable         as SV
import qualified Data.Vector.Storable.Mutable as MV
import           Data.Word

import           Rho.Utils

data Bitfield = Bitfield (MV.IOVector Word8) Int

instance Show Bitfield where
    show (Bitfield _ len) = "<bitfield of length " ++ show len ++ ">"

-- | Create a bitfield of given length.
--
-- >>> availableBits =<< empty 5
-- fromList []
--
-- >>> missingBits =<< empty 5
-- fromList [0,1,2,3,4]
--
empty :: Int -> IO Bitfield
empty len = do
    let (d, m) = len `divMod` 8
    v <- MV.replicate (if m == 0 then d else d + 1) 0
    return $ Bitfield v len

-- | Create a bitfield of given length, with all bits set.
--
-- >>> missingBits =<< full 5
-- fromList []
--
full :: Int -> IO Bitfield
full len = do
    let (d, m) = len `divMod` 8
    v <- MV.replicate (if m == 0 then d else d + 1) 0xFF
    return $ Bitfield v len

length :: Bitfield -> Int
length (Bitfield _ len) = len

-- | Create a bitfield from given bytestring and length.
--
-- >>> availableBits =<< fromBS (B.pack [0, 1]) 16
-- fromList [15]
--
-- >>> availableBits =<< fromBS (B.pack [128, 0]) 16
-- fromList [0]
--
fromBS :: B.ByteString -> Int -> IO Bitfield
fromBS bs len = do
    v <- SV.unsafeThaw (bsToByteVector (B.copy bs))
    return $ Bitfield v len

-- | Create a ByteString from given Bitfield. Extra bits in the generated
-- ByteString will be 0.
toBS :: Bitfield -> IO B.ByteString
toBS (Bitfield v _) = bsFromByteVector `fmap` SV.freeze v

-- | Create a bitfield from given bit indexes and length.
--
-- >>> availableBits =<< fromBitIdxs [1, 3, 5, 10] 10
-- fromList [1,3,5]
--
fromBitIdxs :: [Int] -> Int -> IO Bitfield
fromBitIdxs idxs len = do
    bf@(Bitfield v _) <- empty len
    iter v idxs
    return bf
  where
    iter :: MV.IOVector Word8 -> [Int] -> IO ()
    iter _ [] = return ()
    iter v (i : is)
      | i >= len  = iter v is
      | otherwise = do
          let byteIdx = i `div` 8
              bitIdx  = 7 - (i `mod` 8)
          b <- MV.unsafeRead v byteIdx
          MV.unsafeWrite v byteIdx (B.setBit b bitIdx)
          iter v is

test :: Bitfield -> Int -> IO Bool
test (Bitfield v len) i
  | i > len - 1 =
      error $ "trying to test an invalid bit. (length: " ++ show len ++ ", bit idx: " ++ show i ++ ")"
  | otherwise   = do
      let byteIdx = i `div` 8
          bitIdx  = 7 - (i `mod` 8)
      byte <- MV.unsafeRead v byteIdx
      return $ B.testBit byte bitIdx

set :: Bitfield -> Int -> IO ()
set (Bitfield v len) i
  | i > len - 1 =
      error $ "trying to set an invalid bit. (length: " ++ show len ++ ", bit idx: " ++ show i ++ ")"
  | otherwise   = do
      let byteIdx = i `div` 8
          bitIdx  = 7 - (i `mod` 8)
      byte <- MV.unsafeRead v byteIdx
      MV.unsafeWrite v byteIdx (B.setBit byte bitIdx)

-- | Generate set of indexes of zero bits.
--
-- >>> missingBits =<< fromBS (B.pack [1, 2, 3]) 20
-- fromList [0,1,2,3,4,5,6,8,9,10,11,12,13,15,16,17,18,19]
--
-- >>> missingBits =<< fromBS (B.pack [0xFF]) 8
-- fromList []
--
-- >>> missingBits =<< fromBS (B.pack []) 0
-- fromList []
--
missingBits :: Bitfield -> IO (S.Set Int)
missingBits = collectBits not

-- | Effectively same as checking if `missingBits` is empty, but faster.
--
-- >>> hasMissingBits =<< fromBS (B.pack [0xFF]) 8
-- False
--
-- >>> hasMissingBits =<< fromBS (B.pack [0xFF - 1]) 8
-- True
--
hasMissingBits :: Bitfield -> IO Bool
hasMissingBits bits@(Bitfield bf len) = go 0
  where
    go idx
      | idx == len = return False
      | otherwise  = do
          b <- test bits idx
          if b then go (idx + 1) else return True

-- | Generate set of indexes of one bits.
--
-- >>> availableBits =<< fromBS (B.pack [1, 2, 3]) 20
-- fromList [7,14]
--
-- >>> availableBits =<< fromBS (B.pack [0xFF]) 8
-- fromList [0,1,2,3,4,5,6,7]
--
-- >>> availableBits =<< fromBS (B.pack []) 0
-- fromList []
--
availableBits :: Bitfield -> IO (S.Set Int)
availableBits = collectBits id

-- | Check all bits in range. Out-of-range bits are considered set. Return
-- True if `start` >= `end`.
--
-- >>> (\bf -> checkRange bf 5 10) =<< fromBitIdxs [5..9] 10
-- True
--
checkRange :: Bitfield -> Int -> Int -> IO Bool
checkRange bits@(Bitfield _ len) start end
  | start >= min len end = return True
  | otherwise            = do
      b <- test bits start
      if b then checkRange bits (start + 1) end
           else return False

collectBits :: (Bool -> Bool) -> Bitfield -> IO (S.Set Int)
collectBits p bf@(Bitfield _ len) =
    S.fromList `fmap` filterM (p <.> test bf) [0..len-1]
{-# INLINE collectBits #-}
