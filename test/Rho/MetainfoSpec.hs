{-# LANGUAGE MultiWayIf, StandaloneDeriving, TupleSections #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Rho.MetainfoSpec where

import           Rho.InfoHash
import           Rho.Metainfo
import           Rho.Tracker
import           Rho.Utils

import           Control.Applicative
import           Control.Monad
import           Data.BEncode              as BE
import qualified Data.ByteString           as B
import qualified Data.ByteString.Char8     as BC
import           Data.Either               (isLeft, isRight)
import           Data.Maybe
import           System.Directory          (doesDirectoryExist,
                                            getDirectoryContents)
import           System.FilePath           ((</>))
import           Test.Hspec
import           Test.Hspec.HUnit
import           Test.Hspec.QuickCheck
import           Test.HUnit
import           Test.QuickCheck           hiding (Result)
import           Test.QuickCheck.Instances ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parsing" $ do
    fromHUnitTest $ TestLabel "should parse" shouldParse
    fromHUnitTest $ TestLabel "should not parse" shouldNotParse
    fromHUnitTest $ TestLabel "regressions" regressions

--   describe "parsing-printing" $ do
--     prop "forall d, fromBEncode . toBEncode $ d = Right d" $ \d ->
--       (BE.fromBEncode . BE.toBEncode) (d :: Metainfo) `shouldBe` (Right d)

shouldParse :: Test
shouldParse = TestCase $ do
  tfs <- getFiles "tests/should_parse"
  sequence_ $ flip map tfs $ \(f, c) -> do
    assertBool ("Can't parse torrent file: " ++ f) $ isRight (decode c :: Result Metainfo)

shouldNotParse :: Test
shouldNotParse = TestCase $ do
  tfs <- getFiles "tests/should_not_parse"
  sequence_ $ flip map tfs $ \(f, c) ->
    assertBool ("Parsed a broken torrent file: " ++ f) $ isLeft (decode c :: Result Metainfo)

regressions :: Test
regressions = TestList $ map TestCase [regression1]
  where
    regression1 :: Assertion
    regression1 = do
      let path = "tests/should_parse/archlinux-2014.11.01-dual.iso.torrent"
      mi <- decode <$> B.readFile path
      case mi of
        Left msg -> assertFailure $ "Can't parse " ++ path ++ ": " ++ msg
        Right metainfo -> do
          let info_hash = InfoHash $ B.pack
                [ 0x08, 0x89, 0xCF, 0x68, 0xCF, 0x4A, 0x7A, 0xB7, 0xF1, 0xDB,
                  0x69, 0xC2, 0xFF, 0xAB, 0xE3, 0xDB, 0xFE, 0x53, 0xD0, 0x95 ]
          assertBool "info_hash is wrong" $ iHash (mInfo metainfo) == info_hash

-- | Recursively walk filesystem to collect files.
getFiles :: FilePath -> IO [(FilePath, B.ByteString)]
getFiles root = getDirectoryContents root >>= fmap concat . mapM (\f -> do
  let p = root </> f
  isDir <- doesDirectoryExist p
  if | head f == '.' -> return []
     | isDir -> getFiles p
     | otherwise -> ((:[]) . (p,)) `fmap` B.readFile p)

-- * Arbitrary instances

-- instance Arbitrary Metainfo where
--   arbitrary =
--     liftM5 Metainfo arbitrary arbitrary arbitrary arbitrary arbitrary
--       <*> arbitrary <*> arbitrary
--   shrink = recursivelyShrink
--
-- instance Arbitrary Info where
--   arbitrary = liftM5 Info arbitrary arbitrary (listOf (B.pack <$> vector 20)) arbitrary arbitrary
--   shrink = recursivelyShrink
--
-- instance Arbitrary File where
--   arbitrary = File <$> arbitrary <*> arbitrary <*> arbitrary
--   shrink = recursivelyShrink
--
-- newtype URLString = URLString { unwrapURLString :: String }
--
-- instance Arbitrary URLString where
--     arbitrary = return (URLString "http://test.com:1234/announce") -- TODO: this is not tested yet
--     shrink _ = []
--
-- instance Arbitrary Tracker where
--   arbitrary = oneof
--     [ HTTPTracker . fromJust . parseUrl . unwrapURLString <$> arbitrary
--     ]
--
-- instance Eq Request where
--     r1 == r2 = reqShow r1 == reqShow r2
--
-- deriving instance Eq Tracker
-- deriving instance Eq Metainfo
