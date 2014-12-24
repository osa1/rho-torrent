{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Rho.MagnetSpec where

import           Control.Monad
import qualified Data.ByteString.Char8    as B
import           Data.List
import           Data.Maybe
import           Network.URI

import           Test.Hspec
import           Test.Hspec.Contrib.HUnit
import           Test.Hspec.QuickCheck
import           Test.HUnit
import           Test.QuickCheck

import           Data.Tree.NTree.TypeDefs
import           Network.Browser
import           Network.HTTP
import           Text.XML.HXT.Core

import           Rho.Magnet
import           Rho.PeerCommsSpec        ()
import           Rho.Tracker

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parsing" $ do
    fromHUnitTest $ TestLabel "should parse (from file)" shouldParse
    -- TODO: enable this once the site is up again
    -- fromHUnitTest $ TestLabel "should parse (scraping from ThePirateBay)" (TestCase scrapeMagnets)

  describe "parsing-printing" $ do
    prop "forall m, parseMagnet . printMagnet m == m" $ \m ->
      assertEqual "" (Right m) (parseMagnet (printMagnet m))

instance Arbitrary Magnet where
    arbitrary = do
      xt <- arbitrary
      trs <- listOf trackerGen
      dn <- oneof [return Nothing, return $ Just "display name"]
      return $ Magnet xt trs dn

    shrink (Magnet _ [] _ ) = []
    shrink (Magnet h ts dn) = map (\t -> Magnet h t dn) (init $ subsequences ts)

trackerGen :: Gen Tracker
trackerGen = oneof [http, udp]
  where
    http = return $ HTTPTracker $ fromJust $ parseURI "http://testserver.com:1234/announce"
    udp = return $ UDPTracker "0.0.0.0" (fromIntegral 1234)

shouldParse :: Test
shouldParse = TestCase $ do
  magnetUrls <- B.lines `fmap` B.readFile "tests/magnets_should_parse"
  parseMagnetUrls magnetUrls

parseMagnetUrls :: [B.ByteString] -> Assertion
parseMagnetUrls magnetUrls =
  forM_ magnetUrls $ \magnetUrl ->
    case (parseMagnet magnetUrl) of
      Right Magnet{mTrackers=trackers} ->
        -- TODO: This is not a problem since magnets can omit trackers,
        -- but I couldn't find any torrents like this and we doesn't
        -- support DHT(yet) anyways.
        assertBool ("Can't parse trackers from magnet URL: " ++ B.unpack magnetUrl)
                   (not $ null trackers)
      Left err' ->
        assertFailure $ "Can't parse magnet URL: " ++ B.unpack magnetUrl ++ "\n" ++ err'

-- | Scrape magnet links from ThePirateBay.
scrapeMagnets :: IO ()
scrapeMagnets = do
    putStrLn "Fetching torrent site contents."
    (_, rsp) <- browse $ do
      setOutHandler (const $ return ())
      setAllowRedirects True
      -- hopefully this URL will continue serving HD-movies listing
      request $ getRequest "http://thepiratebay.se/top/207"
    putStrLn "Parsing torrent site contents."
    links <- runX (mkarr (rspBody rsp) >>> tag "a" >>> getAttrValue "href")
    let magnetUrls = map B.pack $ filter ((==) "magnet:?" . take 8) links
    putStrLn $ "Parsing magnet links from torrent site. (" ++ show (length magnetUrls) ++ " urls)"
    parseMagnetUrls magnetUrls
  where
    tag :: ArrowXml a => String -> a XmlTree XmlTree
    tag = multi . hasName

    mkarr :: String -> IOSArrow XmlTree (NTree XNode)
    mkarr = readString [withParseHTML yes, withWarnings no]
