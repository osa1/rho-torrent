{-# LANGUAGE ScopedTypeVariables #-}

module Rho.BitfieldSpec where

import           Control.Applicative
import           Data.Bits
import qualified Data.ByteString           as B
import           Data.List                 (foldl')
import qualified Data.Set                  as S

import           Test.Hspec
import           Test.Hspec.Contrib.HUnit
import           Test.Hspec.QuickCheck
import           Test.HUnit
import           Test.QuickCheck           hiding (Result)
import           Test.QuickCheck.Instances ()

import qualified Rho.Bitfield              as BF

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "bitfield properties" $ do
    modifyMaxSuccess (const 1000) $ prop "random generation and bit checking" $ \bs ->
      let bf    = BF.Bitfield (B.pack bs) (length bs * 8)
          rets  = flip map (zip [0..] bs) $ \(byteIdx, byte) ->
                    flip map [0..7] $ \bitIdx ->
                      BF.test bf (byteIdx * 8 + bitIdx) == testBit byte (7 - bitIdx)
      in all id (concat rets)

    prop "set and test" $ \(BSIndex (bs, idx)) ->
      BF.test (BF.set (BF.Bitfield bs (B.length bs * 8)) idx) idx

    prop "set and collectBits" $ \(BSIndex' (bs, idxs)) ->
      let bs'      = foldl' (\bf i -> BF.setBit bf i) bs (S.toList idxs)
          len      = B.length bs * 8
          missings = BF.missingBits (BF.Bitfield bs' len)
          avails   = BF.availableBits (BF.Bitfield bs' len)
          idxs'    = S.map fromIntegral idxs
      in all id [ idxs' `S.intersection` missings == S.empty
                , idxs' `S.union` missings == S.fromList [0..fromIntegral (B.length bs * 8) - 1]
                , missings `S.intersection` avails == S.empty
                , missings `S.union` avails == S.fromList [0..fromIntegral (B.length bs * 8) - 1]
                ]

  describe "bitfield edge cases (extra trailing zero bits)" $ do
    fromHUnitTest $ TestLabel "extra bits should be ignored" $ TestCase $ do
      let bf       = BF.Bitfield (B.pack [0xFF, 0xFF]) 10
          missings = BF.missingBits bf
          avails   = BF.availableBits bf
      assertBool "missings are not empty" (S.null missings)
      assertEqual "available bits are wrong" (S.fromList [0..9]) avails

    fromHUnitTest $ TestLabel "all bits up to extra bits should count" $ TestCase $ do
      let bf       = BF.Bitfield (B.pack [0xFF, 0x00]) 10
          missings = BF.missingBits bf
          avails   = BF.availableBits bf
      assertEqual "missing bits are wrong" (S.fromList [8, 9]) missings
      assertEqual "available bits are wrong" (S.fromList [0..7]) avails

newtype BSIndex = BSIndex (B.ByteString, Int) deriving (Show)

instance Arbitrary BSIndex where
    arbitrary = do
      idx <- choose (0, 99)
      bs  <- arbitrary `suchThat` (\bs -> B.length bs > idx)
      return $ BSIndex (bs, idx)

newtype BSIndex' = BSIndex' (B.ByteString, S.Set Int) deriving (Show)

instance Arbitrary BSIndex' where
    arbitrary = do
      idxs <- S.fromList <$> listOf1 (choose (0, 99))
      let len = (maximum (S.toList idxs) `div` 8) + 1
          bs  = B.pack $ replicate len 0
      return $ BSIndex' (bs, idxs)
