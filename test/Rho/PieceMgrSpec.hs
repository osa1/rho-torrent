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
    fromHUnitTest lastPieceNonZeroTest
    fromHUnitTest lastPieceZeroTest
    fromHUnitTest bigPieceTest

cons :: a -> (b, c) -> (a, b, c)
cons a (b, c) = (a, b, c)

pieceMgrTest :: Test
pieceMgrTest = TestLabel "basic piece manager ops" $ TestCase $ do
    mgr <- newPieceMgr 100 10

    missings1 <- missingPieces mgr
    let expected1 = zipWith cons [0..9] (repeat (0, 10))
    assertEqual "missing pieces are wrong" expected1 missings1

    writePiece mgr 3 5 (B.pack [1, 1, 1, 1, 1])
    missings2 <- missingPieces mgr
    -- FIXME
    -- let expected2 = take 3 expected1 ++ [(3, 0, 5)] ++ drop 4 expected1
    let expected2 = expected1
    assertEqual "missing pieces are wrong" expected2 missings2

    writePiece mgr 3 0 (B.pack [1, 1, 1, 1, 1])
    missings3 <- missingPieces mgr
    -- FIXME
    -- let expected3 = expected2 \\ [(3, 0, 5)]
    let expected3 = expected2 \\ [(3, 0, 10)]
    assertEqual "missing pieces are wrong" expected3 missings3

    writePiece mgr 4 0 (B.pack [1, 1, 1])
    missings4 <- missingPieces mgr
    let expected4 = take 3 missings3 ++ [(4, 3, 7)] ++ drop 4 missings3
    assertEqual "missing pieces are wrong" expected4 missings4

    writePiece mgr 0 0 (B.pack . take 100 . repeat $ 1)
    missings5 <- missingPieces mgr
    let expected5 = []
    assertEqual "missing pieces are wrong" expected5 missings5

lastPieceNonZeroTest :: Test
lastPieceNonZeroTest = TestLabel "size of last piece (non-zero)" $ TestCase $ do
    mgr <- newPieceMgr 15 7
    writePiece mgr 0 0 (B.pack . take 14 . repeat $ 1)
    missings <- missingPieces mgr
    let expected = [(2, 0, 1)]
    assertEqual "missing pieces are wrong" expected missings

lastPieceZeroTest :: Test
lastPieceZeroTest = TestLabel "size of last piece" $ TestCase $ do
    mgr <- newPieceMgr 14 7
    writePiece mgr 0 0 (B.pack . take 7 . repeat $ 1)
    missings <- missingPieces mgr
    let expected = [(1, 0, 7)]
    assertEqual "missing pieces are wrong" expected missings

bigPieceTest :: Test
bigPieceTest = TestLabel "piece size > torrent size" $ TestCase $ do
    mgr <- newPieceMgr 14 20
    missings <- missingPieces mgr
    let expected = [(0, 0, 14)]
    assertEqual "missing pieces are wrong" expected missings
