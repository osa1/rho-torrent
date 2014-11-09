{-# LANGUAGE OverloadedStrings #-}

module Rho.Magnet where

import qualified Data.ByteString.Char8 as B
import           Data.Maybe            (catMaybes)
import           Network.URI           (unEscapeString)

import           Rho.Tracker

data Magnet = Magnet
  { mHash        :: B.ByteString
    -- ^ URL-encoded, 20-byte info_hash
  , mTrackers    :: [Tracker]
  , mDisplayName :: Maybe String
  } deriving (Show)

parseMagnet :: B.ByteString -> Either String Magnet
parseMagnet bs = do
    let args = parseArgs bs
    case lookup "xt" args of
      Nothing -> Left "Can't parse xt argument from magnet URL."
      Just xt ->
        let dn = lookup "dn" args
            tr = catMaybes . map (either (const Nothing) Just . parseTrackerBS) $
                   -- TODO: redundant bytestring packing/unpacking here
                   map (B.pack . unEscapeString . B.unpack . snd) . filter ((==) "tr" . fst) $ args
        in Right $ Magnet xt tr ((unEscapeString . B.unpack) `fmap` dn)

-- | Parse `a=b` pairs from a query string. Parsing started from the
-- position of '?" in the string.
--
-- >>> parseArgs (B.pack "dummy?a=b&c=d")
-- [("a","b"),("c","d")]
--
-- >>> parseArgs (B.pack "?")
-- []
--
parseArgs :: B.ByteString -> [(B.ByteString, B.ByteString)]
parseArgs =
    -- split to (key, val) pairs
      map (fmap B.tail . B.span (/= '='))
    -- split to key=val strings
    . B.split '&'
    -- drop the prefix
    . B.tail . B.dropWhile (/= '?')
