{-# LANGUAGE ScopedTypeVariables #-}

module Rho.BitfieldSpec where

import           Control.Exception         (ErrorCall, catch)
import           Control.Monad
import           Data.Bits
import qualified Data.ByteString           as B
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
    modifyMaxSuccess (const 1000) $ prop "random generation from bytestring and bit checking" $ \bs ->
      ioProperty $ do
        bf <- BF.fromBS (B.pack bs) (length bs * 8)
        rets <- zipWithM (\byteIdx byte ->
                             forM [0..7] $ \bitIdx -> do
                               bfRet <- BF.test bf (byteIdx * 8 + bitIdx)
                               return $ bfRet == testBit byte (7 - bitIdx)) [0..] bs
        return $ and (concat rets)

    modifyMaxSuccess (const 1000) $ prop "random generation from indexes and bit checking" $
      \(Idxs idxs) -> ioProperty $ do
          let idxs' = S.toList idxs
          bf <- BF.fromBitIdxs idxs' (maximum idxs' + 1)
          avails   <- BF.availableBits bf
          missings <- BF.missingBits bf
          return $ (avails == idxs) .&&. (avails `S.intersection` missings == S.empty)

    modifyMaxSuccess (const 1000) $ prop "set and test" $ \(Idxs idxs) -> ioProperty $ do
      let idxs' = S.toList idxs
      bf <- BF.empty (maximum idxs' + 1)
      forM_ idxs' (BF.set bf)
      tests <- forM idxs' (BF.test bf)
      negTests <- forM [0..maximum idxs'] $ \idx -> if S.member idx idxs
                                                      then return True
                                                      else not <$> BF.test bf idx
      return $ and tests .&&. and negTests

    modifyMaxSuccess (const 1000) $ prop "bytestring conversions" $ \ws ->
      ioProperty $ do
        let bs = B.pack ws
        bf  <- BF.fromBS bs (B.length bs * 8)
        bs' <- BF.toBS bf
        return $ bs' == bs

    fromHUnitTest $ TestLabel "ByteString conversion -- extra bits should be 0" $ TestCase $ do
      bf <- BF.fromBitIdxs [0..8] 9
      bs <- BF.toBS bf
      assertEqual "generated ByteString is wrong" (B.pack [0xFF, 0x80]) bs

    fromHUnitTest $ TestLabel "setting invalid bit should fail with error" $ TestCase $ do
      bf  <- BF.empty 0
      ret <- catch (BF.set bf 0 >> return False) (\(_ :: ErrorCall) -> return True)
      assertBool "set didn't fail" ret

    fromHUnitTest $ TestLabel "getting invalid bit should fail with error" $ TestCase $ do
      bf  <- BF.empty 0
      ret <- catch (BF.test bf 0 >> return False) (\(_ :: ErrorCall) -> return True)
      assertBool "test didn't fail" ret

  describe "bitfield edge cases (extra trailing zero bits)" $ do
    fromHUnitTest $ TestLabel "extra bits should be ignored" $ TestCase $ do
      bf       <- BF.fromBS (B.pack [0xFF, 0xFF]) 10
      missings <- BF.missingBits bf
      avails   <- BF.availableBits bf
      assertBool "missings are not empty" (S.null missings)
      assertEqual "available bits are wrong" (S.fromList [0..9]) avails

    fromHUnitTest $ TestLabel "all bits up to extra bits should count" $ TestCase $ do
      bf       <- BF.fromBS (B.pack [0xFF, 0x00]) 10
      missings <- BF.missingBits bf
      avails   <- BF.availableBits bf
      assertEqual "missing bits are wrong" (S.fromList [8, 9]) missings
      assertEqual "available bits are wrong" (S.fromList [0..7]) avails

  describe "checkRange" $ do
    fromHUnitTest $ TestLabel "checkRange from bitIdxs (out-of-range bits)" $ TestCase $ do
      bf   <- BF.fromBitIdxs [4..20] 10
      ret1 <- BF.checkRange bf 4 30
      assertBool "range check is wrong" ret1

newtype Idxs = Idxs (S.Set Int) deriving (Show)

instance Arbitrary Idxs where
    arbitrary = (Idxs . S.fromList) <$> listOf1 (choose (0, 1000))
