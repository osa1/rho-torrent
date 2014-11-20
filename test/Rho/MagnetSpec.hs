{-# LANGUAGE OverloadedStrings #-}

module Rho.MagnetSpec where

import           Control.Monad
import qualified Data.ByteString.Char8    as B
import           Test.Hspec
import           Test.Hspec.HUnit
import           Test.HUnit

import           Data.Tree.NTree.TypeDefs
import           Network.Browser
import           Network.HTTP
import           Text.XML.HXT.Core

import           Rho.Magnet

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parsing" $ do
    fromHUnitTest $ TestLabel "should parse (from file)" shouldParse
    fromHUnitTest $ TestLabel "should parse (scraping from ThePirateBay)" (TestCase scrapeMagnets)

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
