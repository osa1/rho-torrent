{-# LANGUAGE MultiWayIf, TupleSections #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Rho.MetainfoSpec where

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
import           Rho.TestUtils
import           Rho.Tracker
import           Rho.Utils

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parsing" $ do
    fromHUnitTest $ TestLabel "should parse" shouldParse
    fromHUnitTest $ TestLabel "should not parse" shouldNotParse
    fromHUnitTest $ TestLabel "regressions" regressions
    fromHUnitTest $ TestLabel "InfoHash show instance" infoHashShow

  describe "parsing-printing" $ do
    modifyMaxSuccess (const 100) $ prop "forall d, fromBEncode . toBEncode $ d = Right d" $ \d ->
      ppProp d

shouldParse :: Test
shouldParse = TestCase $ do
  tfs <- getFiles "tests/should_parse"
  forM_ tfs $ \(f, c) ->
    case decode c of
      Left err -> assertFailure ("Can't parse torrent file " ++ f ++ ": " ++ err)
      Right mi -> ppProp mi

shouldNotParse :: Test
shouldNotParse = TestCase $ do
  tfs <- getFiles "tests/should_not_parse"
  forM_ tfs $ \(f, c) ->
    assertBool ("Parsed a broken torrent file: " ++ f) $ isLeft (decode c :: BE.Result Metainfo)

regressions :: Test
regressions = TestList $ map TestCase [regression1]
  where
    regression1 :: Assertion
    regression1 = do
      mi <- parseMIAssertion "tests/should_parse/archlinux-2014.11.01-dual.iso.torrent"
      let info_hash = InfoHash $ B.pack
            [ 0x08, 0x89, 0xCF, 0x68, 0xCF, 0x4A, 0x7A, 0xB7, 0xF1, 0xDB,
              0x69, 0xC2, 0xFF, 0xAB, 0xE3, 0xDB, 0xFE, 0x53, 0xD0, 0x95 ]
      assertEqual "info_hash is wrong" (iHash $ mInfo mi) info_hash
      ppProp mi

infoHashShow :: Test
infoHashShow = TestCase $ do
  mi <- parseMIAssertion "test/test.torrent"
  assertEqual "InfoHash show instance is broken" "f89e0dfd9cfca852d97ad6afa44d8f73ce70b636"
                                                 (show $ iHash $ mInfo mi)

parseMIAssertion :: FilePath -> Assertion' Metainfo
parseMIAssertion miPath = do
    mi <- parseMetainfo <$> B.readFile miPath
    case mi of
      Left err  -> assertFailure' $ "Cant parse " ++ miPath ++ ": " ++ err
      Right mi' -> return mi'

-- | Check property: `BE.fromBEncode . BE.toBEncode` should preserve
-- structure.
ppProp :: Metainfo -> Assertion
ppProp mi = assertEqual "printed info now same with original one"
              (Right mi) (BE.fromBEncode . BE.toBEncode $ mi)

-- | Recursively walk filesystem to collect files.
getFiles :: FilePath -> IO [(FilePath, B.ByteString)]
getFiles root = getDirectoryContents root >>= concat <.> mapM (\f -> do
  let p = root </> f
  isDir <- doesDirectoryExist p
  if | head f == '.' -> return []
     | isDir -> getFiles p
     | otherwise -> ((:[]) . (p,)) `fmap` B.readFile p)

-- * Arbitrary instances

instance Arbitrary Metainfo where
  arbitrary =
    liftM5 Metainfo arbitrary arbitrary arbitrary arbitrary arbitrary
      <*> arbitrary <*> arbitrary
  shrink = recursivelyShrink

instance Arbitrary Info where
  arbitrary = do
    name     <- arbitrary
    pieceLen <- arbitrary
    pieces   <- listOf (B.pack <$> vector 20)
    private  <- arbitrary
    files    <- oneof [Left <$> genSingleFile, Right <$> arbitrary]
    let info  = Info name undefined pieceLen pieces private files
        bv    = BE.toBEncode info
        hash  = mkInfoHash bv
    return info{iHash=hash}

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
