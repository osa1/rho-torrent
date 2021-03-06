{-# LANGUAGE DeriveAnyClass, DeriveGeneric, OverloadedStrings #-}

module Rho.Tracker
  ( Tracker (..)
  , parseTrackerBS
  ) where

import           Control.DeepSeq       (NFData)
import           Data.BEncode
import qualified Data.ByteString.Char8 as B
import           Data.Char             (isNumber)
import           GHC.Generics
import           Network.Socket        (PortNumber)
import           Network.URI

import           Rho.Instances         ()

data Tracker
    = HTTPTracker URI
    | UDPTracker B.ByteString PortNumber
    deriving (Show, Eq, Ord, Generic, NFData)

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
-- >>> parseUDPAddr (B.pack "192.168.1.2:5432")
-- Right ("192.168.1.2",5432)
--
-- >>> parseUDPAddr (B.pack "tracker.openbittorrent.com:1234/announce")
-- Right ("tracker.openbittorrent.com",1234)
--
parseUDPAddr :: B.ByteString -> Either String (B.ByteString, PortNumber)
parseUDPAddr bs =
    let (hostAddr, portStrWColon) = B.span (/= ':') bs in
    case B.uncons portStrWColon of
      Nothing        -> Left $ "Can't parse port number in " ++ B.unpack bs
      Just (_, rest) ->
        let portStr = B.takeWhile isNumber rest in
        case B.readInt portStr of
          Nothing        -> Left $ "Can't parse port number from " ++ B.unpack portStr
          Just (port, _) -> Right (hostAddr, fromIntegral port)
