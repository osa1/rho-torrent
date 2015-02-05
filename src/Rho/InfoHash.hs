{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Rho.InfoHash where

import           Control.DeepSeq (NFData)
import           Data.Bits
import qualified Data.ByteString as B

-- | 20-byte info_hash of torrent
-- TODO: Move this type to somewhere nice and remove this module.
newtype InfoHash = InfoHash { unwrapInfoHash :: B.ByteString } deriving (Eq, Ord, NFData)

instance Show InfoHash where
    show (InfoHash hash) = concatMap toHexDigit $ B.unpack hash
      where
        toHexDigit d = [toHexDigit' (d `shiftR` 4), toHexDigit' (d .&. 0x0F)]

        toHexDigit' 0  = '0'
        toHexDigit' 1  = '1'
        toHexDigit' 2  = '2'
        toHexDigit' 3  = '3'
        toHexDigit' 4  = '4'
        toHexDigit' 5  = '5'
        toHexDigit' 6  = '6'
        toHexDigit' 7  = '7'
        toHexDigit' 8  = '8'
        toHexDigit' 9  = '9'
        toHexDigit' 10 = 'a'
        toHexDigit' 11 = 'b'
        toHexDigit' 12 = 'c'
        toHexDigit' 13 = 'd'
        toHexDigit' 14 = 'e'
        toHexDigit' 15 = 'f'
        toHexDigit' d  = error $ "toHexDigit: invalid digit: " ++ show d
