module Rho.PieceMgrSpec where

import qualified Data.ByteString  as B
import           Data.List
import           Test.Hspec
import           Test.Hspec.HUnit
import           Test.HUnit

import           Rho.PieceMgr

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "piece manager" $ do
    fromHUnitTest pieceMgrTest

pieceMgrTest :: Test
pieceMgrTest = TestLabel "basic piece manager ops" $ TestCase $ do
    mgr <- newPieceMgr 100 10

    missings1 <- missingPieces mgr
    let expected1 = zip [0..9] (repeat 0)
    assertEqual "missing pieces are wrong" expected1 missings1

    writePiece mgr 3 5 (B.pack [1, 1, 1, 1, 1])
    missings2 <- missingPieces mgr
    assertEqual "missing pieces are wrong" expected1 missings2

    writePiece mgr 3 0 (B.pack [1, 1, 1, 1, 1])
    missings3 <- missingPieces mgr
    let expected3 = expected1 \\ [(3, 0)]
    assertEqual "missing pieces are wrong" expected3 missings3

    writePiece mgr 4 0 (B.pack [1, 1, 1])
    missings4 <- missingPieces mgr
    let expected4 = take 3 missings3 ++ [(4, 3)] ++ drop 4 missings3
    assertEqual "missing pieces are wrong" expected4 missings4

    writePiece mgr 0 0 (B.pack . take 100 . repeat $ 1)
    missings5 <- missingPieces mgr
    let expected5 = []
    assertEqual "missing pieces are wrong" expected5 missings5
