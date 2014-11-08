{-# LANGUAGE CPP, MultiWayIf, StandaloneDeriving, TupleSections #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Rho.MetainfoSpec where

import           Rho.Metainfo
import           Rho.Tracker

import           Control.Applicative
import           Control.Monad
import           Data.BEncode              as BE
import qualified Data.ByteString           as B
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

#if MIN_VERSION_base(4,7,0)
import           Data.Either               (isLeft, isRight)
#else
isLeft :: Either a b -> Bool
isLeft Left{} = True
isLeft _      = False

isRight :: Either a b -> Bool
isRight Right{} = True
isRight _       = False
#endif

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parsing" $ do
    fromHUnitTest $ TestLabel "should parse" shouldParse
    fromHUnitTest $ TestLabel "should not parse" shouldNotParse

--   describe "parsing-printing" $ do
--     prop "forall d, fromBEncode . toBEncode $ d = Right d" $ \d ->
--       (BE.fromBEncode . BE.toBEncode) (d :: Metainfo) `shouldBe` (Right d)

shouldParse :: Test
shouldParse = TestCase $ do
  tfs <- getFiles "tests/should_parse"
  sequence_ $ flip map tfs $ \(f, c) -> do
    putStrLn $ "parsing: " ++ f
    assertBool ("Can't parse torrent file: " ++ f) $ isRight (decode c :: Result Metainfo)

shouldNotParse :: Test
shouldNotParse = TestCase $ do
  tfs <- getFiles "tests/should_not_parse"
  sequence_ $ flip map tfs $ \(f, c) ->
    assertBool ("Parsed a broken torrent file: " ++ f) $ isLeft (decode c :: Result Metainfo)

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
