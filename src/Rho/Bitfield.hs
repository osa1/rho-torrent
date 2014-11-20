module Rho.Bitfield where

import qualified Data.Bits       as B
import qualified Data.ByteString as B
import           Data.Maybe
import           Data.Monoid

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
