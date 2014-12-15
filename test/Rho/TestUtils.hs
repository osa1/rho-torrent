module Rho.TestUtils where

import           Control.DeepSeq
import qualified Control.Exception as E
import qualified Data.ByteString   as B
import           Data.List         (foldl')

import           Test.HUnit.Lang
import           Test.QuickCheck

type Assertion' = IO

assertFailure' :: String -> Assertion' a
assertFailure' msg = msg `deepseq` E.throwIO (HUnitFailure msg)

genMaybe :: Gen a -> Gen (Maybe a)
genMaybe g = do
    b <- arbitrary
    if b then Just `fmap` g else return Nothing

ll :: [B.ByteString] -> Int
ll = foldl' (\acc b -> acc + B.length b) 0
