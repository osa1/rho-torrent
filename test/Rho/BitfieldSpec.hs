{-# LANGUAGE ScopedTypeVariables #-}

module Rho.BitfieldSpec where

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
    modifyMaxSuccess (const 1000) $ prop "random testing" $ \bs ->
      let bf    = BF.Bitfield (B.pack bs)
          rets  = flip map (zip [0..] bs) $ \(byteIdx, byte) ->
                    flip map [0..7] $ \bitIdx ->
                      BF.testBit bf (byteIdx * 8 + bitIdx) == testBit byte (7 - bitIdx)
      in all (== True) (concat rets)
