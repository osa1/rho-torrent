{-# LANGUAGE OverloadedStrings #-}

module Rho.Tracker
  ( Tracker (..)
  -- , uriShow
  , parseTrackerBS
  ) where

import           Control.Applicative   ((<$>))
import           Data.BEncode
import qualified Data.ByteString.Char8 as B
import           Data.Monoid
import           Network.Socket        (PortNumber)
import           Network.URI

data Tracker
    = HTTPTracker URI
    | UDPTracker B.ByteString PortNumber
    deriving (Show)

instance BEncode Tracker where
    toBEncode (HTTPTracker uri) = toBEncode . B.pack . show $ uri
    toBEncode (UDPTracker host port) = toBEncode $ mconcat
      [ "udp://", host, ":", B.pack (show port) ]

    fromBEncode (BString bs) = parseTrackerBS bs
    fromBEncode bv = Left $ "Can't parse tracker bencode: " ++ show bv

-- | Try to parse a IP address/host name from given bytestring.
-- We decide on tracker type(UDP or HTTP) using "http://" or "udp://"
-- prefix.
--
-- >>> parseTrackerBS . B.pack $ "http://tracker.opensuse.org:6969/announce"
-- Right (HTTPTracker http://tracker.opensuse.org:6969/announce)
--
-- >>> parseTrackerBS . B.pack $ "udp://192.168.1.2:1234"
-- Right (UDPTracker "192.168.1.2" 1234)
--
-- Note that this functions doesn't check for validity of an URL:
--
-- >>> parseTrackerBS . B.pack $ "http://this_is_not_valid"
-- Right (HTTPTracker http://this_is_not_valid)
--
parseTrackerBS :: B.ByteString -> Either String Tracker
parseTrackerBS bs
  -- I only make an estimation here. I couldn't find anything about format
  -- of tracker fields, and `parseURI` accepts just about everyting.
  | B.take 7 bs == "http://" =
      maybe (Left $ "Can't parse HTTP URL: " ++ B.unpack bs)
            Right
            (HTTPTracker <$> parseURI (B.unpack bs))
  | B.take 6 bs == "udp://"  = uncurry UDPTracker <$> parseUDPAddr (B.drop 6 bs)
  | otherwise                = Left $ "Can't decide whether the host is UDP or HTTP: " ++ B.unpack bs

-- | Try to parse an IP address and port from given bytestring.
--
-- >>> parseUDPAddr (B.pack "192.168.1.2:1234")
-- Right ("192.168.1.2",1234)
--
-- >>> parseUDPAddr (B.pack "192.168.1.2:5432")
-- Right ("192.168.1.2",5432)
--
-- >>> parseUDPAddr (B.pack "tracker.openbittorrent.com:1234")
-- Right ("tracker.openbittorrent.com",1234)
--
parseUDPAddr :: B.ByteString -> Either String (B.ByteString, PortNumber)
parseUDPAddr bs =
    let (hostAddr, portStr) = fmap B.tail $ B.span (/= ':') bs in
    case B.readInt portStr of
      Nothing -> Left $ "Can't parse port number from " ++ B.unpack portStr
      Just (port, rest)
        | not (B.null rest) -> Left $ "Can't parse this part if address string: " ++ B.unpack rest
        | otherwise -> Right (hostAddr, fromIntegral port)
