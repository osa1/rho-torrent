module Rho.Bitfield where

import qualified Data.Bits       as B
import qualified Data.ByteString as B
import           Data.List       (foldl')
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set        as S
import           Data.Word

newtype Bitfield = Bitfield { unwrapBF :: B.ByteString } deriving (Show, Eq)

empty :: Bitfield
empty = Bitfield B.empty

test :: Bitfield -> Int -> Bool
test (Bitfield bs) i = B.testBit (B.index bs (i `div` 8)) (7 - (i `mod` 8))

set :: Bitfield -> Int -> Bitfield
set (Bitfield bytes) i = Bitfield $ setBit bs' i
  where
    len = floor (fromIntegral i / 8 :: Float) + 1
    bs' = if B.length bytes < len then extendBytes bytes (len - B.length bytes) else bytes

extendBytes :: B.ByteString -> Int -> B.ByteString
extendBytes bs extra = bs <> B.replicate extra 0

setBit :: B.ByteString -> Int -> B.ByteString
setBit bs i =
    let byte = i `div` 8
        bit = 7 - (i `mod` 8)
        (prefix, rest) = B.splitAt byte bs
        (rh, rt) = fromJust $ B.uncons rest
    in prefix <> B.cons (B.setBit rh bit) rt

-- | Generate set of indexes of zero bits.
--
-- >>> missingBits (Bitfield $ B.pack [1, 2, 3])
-- fromList [0,1,2,3,4,5,6,8,9,10,11,12,13,15,16,17,18,19,20,21]
--
-- >>> missingBits (Bitfield $ B.pack [0xFF])
-- fromList []
--
-- >>> missingBits (Bitfield $ B.pack [])
-- fromList []
--
missingBits :: Bitfield -> S.Set Word32
missingBits = collectBits (\b i -> not $ B.testBit b i)

-- | Generate set of indexes of one bits.
--
-- >>> availableBits (Bitfield $ B.pack [1, 2, 3])
-- fromList [7,14,22,23]
--
-- >>> availableBits (Bitfield $ B.pack [0xFF])
-- fromList [0,1,2,3,4,5,6,7]
--
-- >>> availableBits (Bitfield $ B.pack [])
-- fromList []
--
availableBits :: Bitfield -> S.Set Word32
availableBits = collectBits B.testBit

collectBits :: (Word8 -> Int -> Bool) -> Bitfield -> S.Set Word32
collectBits p (Bitfield bf) = go bf 0 S.empty
  where
    go :: B.ByteString -> Word32 -> S.Set Word32 -> S.Set Word32
    go bf byteIdx s =
      case B.uncons bf of
        Nothing -> s
        Just (w8, bf') ->
          let s' = foldl' (\s' bitIdx -> if p w8 (7 - bitIdx)
                                           then S.insert (byteIdx * 8 + fromIntegral bitIdx) s'
                                           else s') s [0..7]
          in go bf' (byteIdx + 1) s'
{-# INLINE collectBits #-}
