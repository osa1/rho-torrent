module Rho.Bitfield where

import           Data.Bits                   as B
import qualified Data.ByteString             as B

newtype Bitfield = Bitfield B.ByteString deriving (Show, Eq)

testBit :: Bitfield -> Int -> Bool
testBit (Bitfield bs) i = B.testBit (B.index bs (i `div` 8)) (7 - (i `mod` 8))
