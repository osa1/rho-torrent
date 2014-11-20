{-# LANGUAGE ScopedTypeVariables #-}

module Rho.BitfieldSpec where

import           Control.Applicative
import           Control.Monad
import           Data.Bits
import qualified Data.ByteString           as B
import           Data.Word
import           Test.Hspec
import           Test.Hspec.HUnit
import           Test.Hspec.QuickCheck
import           Test.HUnit
import           Test.QuickCheck           hiding (Result)
import           Test.QuickCheck.Instances ()

import qualified Rho.Bitfield              as BF

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "bitfield" $ do
    modifyMaxSuccess (const 1000) $ prop "random generation and bit checking" $ \bs ->
      let bf    = BF.Bitfield (B.pack bs)
          rets  = flip map (zip [0..] bs) $ \(byteIdx, byte) ->
                    flip map [0..7] $ \bitIdx ->
                      BF.test bf (byteIdx * 8 + bitIdx) == testBit byte (7 - bitIdx)
      in all (== True) (concat rets)

    prop "setBit and test" $ \(BSIndex (bs, idx)) ->
      BF.test (BF.Bitfield (BF.setBit bs idx)) idx

    prop "set and test" $ \(bs, idx :: Word16) ->
      let i = fromIntegral idx
      in BF.test (BF.set (BF.Bitfield bs) i) i

newtype BSIndex = BSIndex (B.ByteString, Int) deriving (Show)

instance Arbitrary BSIndex where
    arbitrary = do
      idx <- arbitrary `suchThat` (\i -> i >= 0 && i < 100)
      bs  <- arbitrary `suchThat` (\bs -> B.length bs > idx)
      return $ BSIndex (bs, idx)
