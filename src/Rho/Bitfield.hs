module Rho.Bitfield where

import qualified Data.Bits       as B
import qualified Data.ByteString as B
import           Data.List       (foldl')
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set        as S
import           Data.Word

data Bitfield = Bitfield B.ByteString Int deriving (Show, Eq)

empty :: Int -> Bitfield
empty len = let (d, m) = len `divMod` 8
            in Bitfield (B.pack $ replicate (if m == 0 then d else d + 1) 0) len

test :: Bitfield -> Int -> Bool
test (Bitfield bs len) i
  | i > len - 1 =
      error $ "trying to test an invalid bit. (length: " ++ show len ++ ", bit idx: " ++ show i ++ ")"
  | otherwise   = B.testBit (B.index bs (i `div` 8)) (7 - (i `mod` 8))

set :: Bitfield -> Int -> Bitfield
set (Bitfield bytes len) i
  | i > len - 1 =
      error $ "trying to set an invalid bit. (length: " ++ show len ++ ", bit idx: " ++ show i ++ ")"
  | otherwise   = Bitfield (setBit bytes i) len

setBit :: B.ByteString -> Int -> B.ByteString
setBit bs i =
    let byte = i `div` 8
        bit = 7 - (i `mod` 8)
        (prefix, rest) = B.splitAt byte bs
        (rh, rt) = fromJust $ B.uncons rest
    in prefix <> B.cons (B.setBit rh bit) rt

-- | Generate set of indexes of zero bits.
--
-- >>> missingBits (Bitfield (B.pack [1, 2, 3]) 20)
-- fromList [0,1,2,3,4,5,6,8,9,10,11,12,13,15,16,17,18,19]
--
-- >>> missingBits (Bitfield (B.pack [0xFF]) 8)
-- fromList []
--
-- >>> missingBits (Bitfield (B.pack []) 0)
-- fromList []
--
missingBits :: Bitfield -> S.Set Word32
missingBits = collectBits not

-- | Generate set of indexes of one bits.
--
-- >>> availableBits (Bitfield (B.pack [1, 2, 3]) 20)
-- fromList [7,14]
--
-- >>> availableBits (Bitfield (B.pack [0xFF]) 8)
-- fromList [0,1,2,3,4,5,6,7]
--
-- >>> availableBits (Bitfield (B.pack []) 0)
-- fromList []
--
availableBits :: Bitfield -> S.Set Word32
availableBits = collectBits id

collectBits :: (Bool -> Bool) -> Bitfield -> S.Set Word32
collectBits p bf@(Bitfield _ len) =
    S.fromList . map (fromIntegral . fst) . filter snd . zip [0..len-1] $ map (p . test bf) [0..len-1]
{-# INLINE collectBits #-}
