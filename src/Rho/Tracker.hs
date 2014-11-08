{-# LANGUAGE OverloadedStrings #-}

module Rho.Tracker
  ( Tracker (..)
  -- , uriShow
  , parseTrackerBS
  ) where

import           Control.Applicative              ((<$>))
import           Data.Attoparsec.ByteString.Char8
import           Data.BEncode
import           Data.Bits                        (shiftL)
import qualified Data.ByteString.Char8            as B
import           Data.Monoid
import           Data.Word
import           Network.Socket                   (HostAddress, PortNumber,
                                                   SockAddr (..))
import           Network.URI

data Tracker
    = HTTPTracker URI
    -- We need a `SockAddr` to be able to `sendTo` using sockets,
    -- however, most of the time host names are used in torrents or
    -- magnets. Since IO operation is needed to resolve IP address of an
    -- host name, I'm keeping the parser pure and leaving host names
    -- unresolved here.
    | UDPTracker (Either (B.ByteString, PortNumber) SockAddr)
    deriving (Show)

instance BEncode Tracker where
    toBEncode (HTTPTracker uri) = toBEncode . B.pack . show $ uri
    toBEncode (UDPTracker (Left (host, port))) = toBEncode $ mconcat
      [ "udp://", host, ":", B.pack (show port) ]
    toBEncode (UDPTracker (Right sock)) = toBEncode $ B.pack $ show sock

    fromBEncode (BString bs) =
      maybe (Left $ "Can't parse tracker bencode: " ++ B.unpack bs)
            Right
            (parseTrackerBS bs)
    fromBEncode bv = Left $ "Can't parse tracker bencode: " ++ show bv

-- | Try to parse a host address from given bytestring.
--
-- >>> parseTrackerBS . B.pack $ "http://tracker.opensuse.org:6969/announce"
-- Just (HTTPTracker http://tracker.opensuse.org:6969/announce)
--
-- >>> parseTrackerBS . B.pack $ "udp://192.168.1.2:1234"
-- Just (UDPTracker (Right 2.1.168.192:1234))
--
parseTrackerBS :: B.ByteString -> Maybe Tracker
parseTrackerBS bs
  -- I only make an estimation here. I couldn't find anything about format
  -- of tracker fields, and `parseURI` accepts just about everyting.
  | B.take 7 bs == "http://" = HTTPTracker <$> parseURI (B.unpack bs)
  | B.take 6 bs == "udp://"  = UDPTracker <$> parseUDPAddr (B.drop 6 bs)
  | otherwise                = Nothing

-- | Try to parse an UDP address from given bytestring.
--
-- >>> parseUDPAddr (B.pack "udp://192.168.1.2:1234")
-- Just (Right 2.1.168.192:1234)
--
-- >>> parseUDPAddr (B.pack "192.168.1.2:5432")
-- Just (Right 2.1.168.192:5432)
--
-- >>> parseUDPAddr (B.pack "udp://tracker.openbittorrent.com:1234")
-- Just (Left ("tracker.openbittorrent.com",1234))
--
parseUDPAddr :: B.ByteString -> Maybe (Either (B.ByteString, PortNumber) SockAddr)
parseUDPAddr bs =
    flip fmap (maybeResult $ feed (parse udpParser bs) B.empty) $ \(port, r) ->
      case r of
        Left bs' -> Left (bs', port)
        Right ip -> Right (SockAddrInet port ip)
  where
    udpParser :: Parser (PortNumber, Either B.ByteString HostAddress)
    udpParser = do
      option () (string "udp://" >> return ())
      bytes <- option Nothing $ Just <$> (decimal :: Parser Word32) `sepBy1` char '.'
      case bytes of
        Just [b1, b2, b3, b4] -> do
          port <- parsePort
          endOfInput
          return ( port
                 , Right $   b1 `shiftL` 24
                           + b2 `shiftL` 16
                           + b3 `shiftL` 8
                           + b4
                 )
        Just _ -> fail "Can't parse TCPv4 address"
        Nothing -> do
          host <- takeTill (== ':')
          port <- parsePort
          return (port, Left host)

    parsePort :: Parser PortNumber
    parsePort = fromIntegral <$> (char ':' >> (decimal :: Parser Word16))
