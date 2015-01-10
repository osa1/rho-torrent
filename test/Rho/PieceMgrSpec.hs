{-# LANGUAGE OverloadedStrings #-}

module Rho.PieceMgrSpec where

import           Control.Monad
import qualified Data.ByteString          as B
import           Data.List

import           Test.Hspec
import           Test.Hspec.Contrib.HUnit
import           Test.HUnit

import           Rho.Metainfo
import           Rho.MetainfoSpec         (parseMIAssertion)
import           Rho.PeerComms.Message
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
    fromHUnitTest testTorrentPieceTest
    fromHUnitTest testNextMissingPart
    fromHUnitTest testGetPieceData
    fromHUnitTest testReadFiles

pieceMgrTest :: Test
pieceMgrTest = TestLabel "basic piece manager ops" $ TestCase $ do
    mgr <- newPieceMgr 100 10

    missings1 <- missingPieces mgr
    let expected1 = [0..9]
    assertEqual "missing pieces are wrong" expected1 missings1

    newBytes1 <- writePiece mgr 3 5 (B.pack [1, 1, 1, 1, 1])
    assertEqual "writePiece ret is wrong" 5 newBytes1
    missings2 <- missingPieces mgr
    let expected2 = expected1
    assertEqual "missing pieces are wrong" expected2 missings2

    newBytes2 <- writePiece mgr 3 0 (B.pack [1, 1, 1, 1, 1])
    assertEqual "writePiece ret is wrong" 5 newBytes2
    missings3 <- missingPieces mgr
    let expected3 = expected2 \\ [3]
    assertEqual "missing pieces are wrong" expected3 missings3

    newBytes3 <- writePiece mgr 4 0 (B.pack [1, 1, 1])
    assertEqual "writePiece ret is wrong" 3 newBytes3
    missings4 <- missingPieces mgr
    let expected4 = expected3
    assertEqual "missing pieces are wrong" expected4 missings4

    newBytes4 <- writePiece mgr 0 0 (B.pack . take 100 . repeat $ 1)
    assertEqual "writePiece ret is wrong" 87 newBytes4
    missings5 <- missingPieces mgr
    let expected5 = []
    assertEqual "missing pieces are wrong" expected5 missings5

lastPieceNonZeroTest :: Test
lastPieceNonZeroTest = TestLabel "size of last piece (non-zero)" $ TestCase $ do
    mgr <- newPieceMgr 15 7
    newBytes <- writePiece mgr 0 0 (B.pack . take 14 . repeat $ 1)
    assertEqual "writePiece ret is wrong" 14 newBytes
    missings <- missingPieces mgr
    assertEqual "missing pieces are wrong" [2] missings

lastPieceZeroTest :: Test
lastPieceZeroTest = TestLabel "size of last piece" $ TestCase $ do
    mgr <- newPieceMgr 14 7
    newBytes <- writePiece mgr 0 0 (B.pack . take 7 . repeat $ 1)
    assertEqual "writePiece ret is wrong" 7 newBytes
    missings <- missingPieces mgr
    assertEqual "missing pieces are wrong" [1] missings

bigPieceTest :: Test
bigPieceTest = TestLabel "piece size > torrent size" $ TestCase $ do
    mgr <- newPieceMgr 14 20
    missings <- missingPieces mgr
    assertEqual "missing pieces are wrong" [0] missings

testTorrentPieceTest :: Test
testTorrentPieceTest = TestLabel "test.torrent pieces" $ TestCase $ do
    mi <- parseMIAssertion "test/test.torrent"
    pieceMgr <- newPieceMgr (fromIntegral $ torrentSize $ mInfo mi)
                            (fromIntegral $ iPieceLength $ mInfo mi)
    missings <- missingPieces pieceMgr
    assertEqual "missing pieces are wrong" [0] missings
    pieceMsg <- B.readFile "test/test_data/test_torrent_piece"
    case parsePeerMsg pieceMsg of
      Left err -> assertFailure $ "Can't parse piece message: " ++ err
      Right (Piece pIdx pOffset pData) -> do
        void $ writePiece pieceMgr pIdx pOffset pData
        missings1 <- missingPieces pieceMgr
        assertEqual "missing pieces are wrong" [] missings1
        check <- checkPieces pieceMgr 0 (head $ iPieces $ mInfo mi)
        assertBool "piece hash check failed" check
        files <- generateFiles pieceMgr (mInfo mi)
        let expectedFiles =
              [ ("seed_files/file1.txt", "file1\n")
              , ("seed_files/file2.txt", "file2\n")
              ]
        assertEqual "generated files are wrong" expectedFiles files
        d <- getPieceData pieceMgr 0 0 20
        assertEqual "returned piece data is wrong" (Just pData) d
      Right msg -> assertFailure $ "Parsed piece message to somehing else: " ++ show msg

testNextMissingPart :: Test
testNextMissingPart = TestList
  [ TestLabel "nextMissingPart - 1" $ TestCase $ do
      mgr <- newPieceMgr 50 10
      missing1 <- nextMissingPart mgr 1
      assertEqual "next missing part is wrong" (Just (0, 10)) missing1
      void $ writePiece mgr 1 3 (B.pack [0, 0, 0])
      missing2 <- nextMissingPart mgr 1
      assertEqual "next missing part is wrong" (Just (0, 3)) missing2
      void $ writePiece mgr 1 0 (B.pack [0, 0, 0])
      missing3 <- nextMissingPart mgr 1
      assertEqual "next missing part is wrong" (Just (6, 4)) missing3
      void $ writePiece mgr 1 6 (B.pack [0, 0, 0, 0])
      missing4 <- nextMissingPart mgr 1
      assertEqual "next missing part is wrong" Nothing missing4
  , TestLabel "nextMissingPart - regression" $ TestCase $ do
      mgr <- newPieceMgr 12 16384
      missing1 <- nextMissingPart mgr 0
      assertEqual "next missing part is wrong" (Just (0, 12)) missing1
      void $ writePiece mgr 0 0 (B.replicate 12 0)
      missing2 <- nextMissingPart mgr 0
      assertEqual "next missing part is wrong" Nothing missing2
  ]

testGetPieceData :: Test
testGetPieceData = TestList
  [ TestLabel "getPieceData - 1" $ TestCase $ do
      pieceMgr <- newPieceMgr 100 10
      pd1 <- getPieceData pieceMgr 3 5 5
      assertEqual "returned piece data is wrong" Nothing pd1
      void $ writePiece pieceMgr 3 5 (B.pack [1])
      pd2 <- getPieceData pieceMgr 3 5 5
      assertEqual "returned piece data is wrong" Nothing pd2
      void $ writePiece pieceMgr 3 6 (B.pack [1, 1, 1])
      pd3 <- getPieceData pieceMgr 3 5 5
      assertEqual "returned piece data is wrong" Nothing pd3
      void $ writePiece pieceMgr 3 9 (B.pack [1])
      pd4 <- getPieceData pieceMgr 3 5 5
      assertEqual "returned piece data is wrong" (Just $ B.pack [1,1,1,1,1]) pd4
  , TestLabel "getPieceData - smallar response than requested" $ TestCase $ do
      pieceMgr <- newPieceMgr 10 10
      void $ writePiece pieceMgr 0 0 (B.pack $ replicate 10 1)
      pd <- getPieceData pieceMgr 0 5 10
      assertEqual "returned piece data is wrong" (Just $ B.pack $ replicate 5 1) pd
  , TestLabel "getPieceData - wrong range" $ TestCase $ do
      pieceMgr <- newPieceMgr 10 10
      void $ writePiece pieceMgr 0 0 (B.pack $ replicate 10 1)
      pd <- getPieceData pieceMgr 0 10 10
      assertEqual "returned piece data is wrong" Nothing pd
  ]

testReadFiles :: Test
testReadFiles = TestList
  [ TestLabel "tryReadFiles - 1" $ TestCase $ do
      Metainfo{mInfo=info} <- parseMIAssertion "test/test.torrent"
      (pieces, filled) <- tryReadFiles info "test"
      assertBool "Piece manager is not filled" filled
      missings <- missingPieces pieces
      assertEqual "Missing pieces are not empty" [] missings
      files <- generateFiles pieces info
      let expectedFiles =
            [ ("seed_files/file1.txt", "file1\n")
            , ("seed_files/file2.txt", "file2\n")
            ]
      assertEqual "Generated files are wrong" expectedFiles files
  ]
