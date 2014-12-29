module Rho.BitfieldSpec where

import           Control.Applicative
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

newtype Idxs = Idxs (S.Set Int) deriving (Show)

instance Arbitrary Idxs where
    arbitrary = (Idxs . S.fromList) <$> listOf1 (choose (0, 1000))
