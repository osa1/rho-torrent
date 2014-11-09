{-# LANGUAGE OverloadedStrings #-}

module Rho.Magnet where

import           Data.Bits               (shiftL)
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8   as B
import qualified Data.ByteString.Lazy    as LB
import           Data.Char               (digitToInt)
import           Data.Maybe              (catMaybes)
import           Data.Monoid
import           Network.URI             (unEscapeString)

import           Rho.Tracker

data Magnet = Magnet
  { mHash        :: B.ByteString
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
            xt' = parseInfoHash (B.drop 9 xt) -- drop "urn:btih:" prefix and parse
        in Right $ Magnet xt' tr ((unEscapeString . B.unpack) `fmap` dn)

-- | Parse character representation of info hash(e.g. hex notation, two
-- chars per byte) to byte representation.
--
-- TODO: We probably need some error handling here.
parseInfoHash :: B.ByteString -> B.ByteString
parseInfoHash = LB.toStrict . BB.toLazyByteString . go
 where
   go bs =
     case B.uncons bs of
       Nothing -> mempty
       Just (c1, rest) ->
         case B.uncons rest of
           Nothing -> error "error while parsing info hash"
           Just (c2, rest') ->
             (BB.word8 $ fromIntegral $ (digitToInt c1 `shiftL` 4) + digitToInt c2) <> go rest'

-- | Parse `a=b` pairs from a query string. Parsing starts from the
-- position of '?' in the string.
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
