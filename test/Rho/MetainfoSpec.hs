{-# LANGUAGE MultiWayIf, StandaloneDeriving, TupleSections #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Rho.MetainfoSpec where

import           Control.Applicative
import           Control.Monad
import           Data.BEncode              as BE
import qualified Data.ByteString           as B
import           Data.Either               (isLeft)
import           System.Directory          (doesDirectoryExist,
                                            getDirectoryContents)
import           System.FilePath           ((</>))

import           Test.Hspec
import           Test.Hspec.Contrib.HUnit
import           Test.Hspec.QuickCheck
import           Test.HUnit
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           Rho.InfoHash
import           Rho.MagnetSpec            (trackerGen)
import           Rho.Metainfo
import           Rho.Tracker

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parsing" $ do
    fromHUnitTest $ TestLabel "should parse" shouldParse
    fromHUnitTest $ TestLabel "should not parse" shouldNotParse
    fromHUnitTest $ TestLabel "regressions" regressions

  describe "parsing-printing" $ do
    modifyMaxSuccess (const 100) $ prop "forall d, fromBEncode . toBEncode $ d = Right d" $ \d ->
      ppProp d

shouldParse :: Test
shouldParse = TestCase $ do
  tfs <- getFiles "tests/should_parse"
  sequence_ $ flip map tfs $ \(f, c) -> do
    case decode c of
      Left err -> assertFailure ("Can't parse torrent file " ++ f ++ ": " ++ err)
      Right mi -> ppProp mi

shouldNotParse :: Test
shouldNotParse = TestCase $ do
  tfs <- getFiles "tests/should_not_parse"
  sequence_ $ flip map tfs $ \(f, c) ->
    assertBool ("Parsed a broken torrent file: " ++ f) $ isLeft (decode c :: BE.Result Metainfo)

regressions :: Test
regressions = TestList $ map TestCase [regression1]
  where
    regression1 :: Assertion
    regression1 = do
      let torrentPath = "tests/should_parse/archlinux-2014.11.01-dual.iso.torrent"
      mi <- decode <$> B.readFile torrentPath
      case mi of
        Left msg -> assertFailure $ "Can't parse " ++ torrentPath ++ ": " ++ msg
        Right metainfo -> do
          let info_hash = InfoHash $ B.pack
                [ 0x08, 0x89, 0xCF, 0x68, 0xCF, 0x4A, 0x7A, 0xB7, 0xF1, 0xDB,
                  0x69, 0xC2, 0xFF, 0xAB, 0xE3, 0xDB, 0xFE, 0x53, 0xD0, 0x95 ]
          assertEqual "info_hash is wrong" (mInfoHash metainfo) info_hash
          ppProp metainfo

-- | Check property: `BE.fromBEncode . BE.toBEncode` should preserve
-- structure.
ppProp :: Metainfo -> Assertion
ppProp mi = assertEqual "printed info now same with original one"
              (Right mi) (BE.fromBEncode . BE.toBEncode $ mi)

-- | Recursively walk filesystem to collect files.
getFiles :: FilePath -> IO [(FilePath, B.ByteString)]
getFiles root = getDirectoryContents root >>= fmap concat . mapM (\f -> do
  let p = root </> f
  isDir <- doesDirectoryExist p
  if | head f == '.' -> return []
     | isDir -> getFiles p
     | otherwise -> ((:[]) . (p,)) `fmap` B.readFile p)

-- * Arbitrary instances

instance Arbitrary Metainfo where
  arbitrary = do
    info <- arbitrary
    let infoHash = mkInfoHash $ toBEncode info
    liftM5 Metainfo arbitrary arbitrary arbitrary arbitrary arbitrary
      <*> arbitrary <*> pure infoHash <*> pure info
  shrink = recursivelyShrink

instance Arbitrary Info where
  arbitrary = do
    singleFile <- arbitrary
    files <- if singleFile then Left <$> genSingleFile else Right <$> arbitrary
    Info <$> arbitrary <*> arbitrary <*> listOf (B.pack <$> vector 20)
         <*> arbitrary <*> pure files
  shrink _ = []

genSingleFile :: Gen File
genSingleFile = File <$> arbitrary <*> arbitrary <*> pure []

genMultiFile :: Gen [File]
genMultiFile = arbitrary

instance Arbitrary File where
  arbitrary = File <$> arbitrary <*> arbitrary <*> listOf (arbitrary `suchThat` (not . B.null))
  shrink = recursivelyShrink

instance Arbitrary Tracker where
  arbitrary = trackerGen
