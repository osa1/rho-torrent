{-# LANGUAGE OverloadedStrings #-}

module Rho.Magnet where

import           Data.Bits
import qualified Data.ByteString         as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8   as BC
import qualified Data.ByteString.Lazy    as LB
import           Data.Char               (digitToInt, intToDigit)
import           Data.Maybe              (mapMaybe)
import           Data.Monoid
import           Data.Word               (Word8)
import           Network.URI             (unEscapeString)

import           Rho.InfoHash
import           Rho.Tracker

data Magnet = Magnet
  { mHash        :: InfoHash
  , mTrackers    :: [Tracker]
  , mDisplayName :: Maybe String
  } deriving (Show, Eq)

parseMagnet :: B.ByteString -> Either String Magnet
parseMagnet bs = do
    let args = parseArgs bs
    case lookup "xt" args of
      Nothing -> Left "Can't parse xt argument from magnet URL."
      Just xt ->
        let dn = lookup "dn" args
            tr = mapMaybe (either (const Nothing) Just . parseTrackerBS) $
                   -- TODO: redundant bytestring packing/unpacking here
                   map (BC.pack . unEscapeString . BC.unpack . snd) . filter ((==) "tr" . fst) $ args
            xt' = parseInfoHash (B.drop 9 xt) -- drop "urn:btih:" prefix and parse
        in Right $ Magnet xt' tr ((unEscapeString . BC.unpack) `fmap` dn)

printMagnet :: Magnet -> B.ByteString
printMagnet (Magnet (InfoHash bs) trs dn) = LB.toStrict . BB.toLazyByteString . mconcat $
    [ BB.byteString "magnet:?xt=urn:btih:"
    , BB.byteString $ bsEncodeBytes bs
    , maybe mempty (BB.byteString . ("&dn=" <>) . BC.pack) dn
    ] <> map (BB.byteString . ("&tr=" <>) . printTracker) trs
  where
    bsEncodeBytes :: B.ByteString -> B.ByteString
    bsEncodeBytes = BC.pack . encodeBytes . B.unpack

    encodeBytes :: [Word8] -> String
    encodeBytes [] = ""
    encodeBytes (w : ws) =
      let ln = w .&. 0x0F
          hn = w `shiftR` 4
      in intToDigit (fromIntegral hn) : intToDigit (fromIntegral ln) : encodeBytes ws

    printTracker :: Tracker -> B.ByteString
    printTracker (HTTPTracker uri) = BC.pack $ show uri
    printTracker (UDPTracker host port) = "udp://" <> host <> ":" <> BC.pack (show port)

-- | Parse character representation of info hash(e.g. hex notation, two
-- chars per byte) to byte representation.
--
-- TODO: We probably need some error handling here. Make sure info_hash is
-- 20-byte long.
parseInfoHash :: B.ByteString -> InfoHash
parseInfoHash = InfoHash . LB.toStrict . BB.toLazyByteString . go
 where
   go bs =
     case BC.uncons bs of
       Nothing -> mempty
       Just (c1, rest) ->
         case BC.uncons rest of
           Nothing -> error "error while parsing info hash"
           Just (c2, rest') ->
             (BB.word8 $ fromIntegral $ (digitToInt c1 `shiftL` 4) + digitToInt c2) <> go rest'

-- | Parse `a=b` pairs from a query string. Parsing starts from the
-- position of '?' in the string.
--
-- >>> parseArgs (BC.pack "dummy?a=b&c=d")
-- [("a","b"),("c","d")]
--
-- >>> parseArgs (BC.pack "?")
-- []
--
parseArgs :: B.ByteString -> [(B.ByteString, B.ByteString)]
parseArgs =
    -- split to (key, val) pairs
      map (fmap BC.tail . BC.span (/= '='))
    -- split to key=val strings
    . BC.split '&'
    -- drop the prefix
    . BC.tail . BC.dropWhile (/= '?')
