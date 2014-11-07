{-# LANGUAGE OverloadedStrings #-}

module Rho.Tracker
  ( Tracker (..)
  , reqShow
  , parseTrackerBS
  ) where

import           Control.Applicative              ((<$>), (<|>))
import           Data.Attoparsec.ByteString
import           Data.Attoparsec.ByteString.Char8
import           Data.BEncode
import           Data.Bits                        (shiftL)
import qualified Data.ByteString.Char8            as B
import           Data.Monoid
import           Data.Word
import           Network.HTTP.Client
import           Network.Socket                   (HostAddress, PortNumber,
                                                   SockAddr (..))

data Tracker
    = HTTPTracker Request
    | UDPTracker SockAddr
    deriving (Show)

reqShow :: Request -> B.ByteString
reqShow req =
    "http://" <> host req <> ":" <> B.pack (show (port req)) <> path req <> queryString req

instance BEncode Tracker where
    toBEncode (HTTPTracker req) = toBEncode $ reqShow req
    toBEncode (UDPTracker sock) = toBEncode . B.pack $ show sock
    fromBEncode (BString bs) =
      maybe (Left $ "Can't parse tracker bencode: " ++ B.unpack bs)
            Right
            (parseTrackerBS bs)
    fromBEncode bv = Left $ "Can't parse tracker bencode: " ++ show bv

-- | Try to parse a host address from given bytestring.
--
-- >>> :m + Data.Maybe Network.HTTP.Client
-- >>> :{
-- let ret = (\(HTTPTracker t) -> t) $
--             fromJust (parseTrackerBS (B.pack "http://tracker.opensuse.org:6969/announce"))
-- :}
--
-- >>> host ret
-- "tracker.opensuse.org"
--
-- >>> port ret
-- 6969
--
-- >>> parseTrackerBS (B.pack "udp://192.168.1.2:1234")
-- Just (UDPTracker 2.1.168.192:1234)
--
parseTrackerBS :: B.ByteString -> Maybe Tracker
parseTrackerBS bs =
        (HTTPTracker <$> parseUrl (B.unpack bs))
    <|> (UDPTracker . uncurry SockAddrInet <$> parseUDPAddr bs)

-- | Try to parse an UDP address from given bytestring.
--
-- >>> parseUDPAddr (B.pack "udp://192.168.1.2:1234")
-- Just (1234,3232235778)
-- >>> parseUDPAddr (B.pack "192.168.1.2:5432")
-- Just (5432,3232235778)
--
parseUDPAddr :: B.ByteString -> Maybe (PortNumber, HostAddress)
parseUDPAddr bs = maybeResult $ feed (parse udpParser bs) B.empty
  where
    udpParser :: Parser (PortNumber, HostAddress)
    udpParser = do
      option () (string "udp://" >> return ())
      bytes <- (decimal :: Parser Word32) `sepBy` char '.' <?> "IPv4 address"
      case bytes of
        [b1, b2, b3, b4] -> do
          port <- fromIntegral <$> (char ':' >> (decimal :: Parser Word16))
          endOfInput
          return ( port
                 ,   b1 `shiftL` 24
                   + b2 `shiftL` 16
                   + b3 `shiftL` 8
                   + b4
                 )
        _ -> fail "Can't parse TCPv4 address"
