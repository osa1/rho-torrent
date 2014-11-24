-- | ... because every project needs one.
module Rho.Utils where

import           Control.Applicative
import qualified Data.BEncode          as BE
import qualified Data.BEncode.BDict    as BE
import qualified Data.BEncode.Types    as BE
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BC
import           Data.Char
import           Data.Word
import           Network.Socket        (PortNumber (..), SockAddr (..))

import           Rho.Parser

-- | `urlEncode` takes a UTF-8 string. This is becoming a problem while
-- encoding bytes:
--
-- > urlEncode $ BC.unpack $ B.pack [0x89]
-- "%C2%89"
--
-- ... it should have been "%89". To avoid this, we modify `urlEncode` to
-- work on bytes.
--
-- (According to Wikipedia article
-- http://en.wikipedia.org/wiki/Percent-encoding we should not encode
-- unreserved characters as bytes)
--
-- >>> :m + Data.ByteString
-- >>> :{
-- urlEncodeBytes $ pack
--   [ 0x08, 0x89, 0xCF, 0x68, 0xCF, 0x4A, 0x7A, 0xB7, 0xF1, 0xDB,
--     0x69, 0xC2, 0xFF, 0xAB, 0xE3, 0xDB, 0xFE, 0x53, 0xD0, 0x95 ]
-- :}
-- "%08%89%CFh%CFJz%B7%F1%DBi%C2%FF%AB%E3%DB%FES%D0%95"
--
urlEncodeBytes :: B.ByteString -> String
urlEncodeBytes bs =
    case BC.uncons bs of
      Nothing -> ""
      Just (ch, t)
        | (isAscii ch && isAlphaNum ch) || ch `elem` "-_.~" -> ch : urlEncodeBytes t
        | otherwise -> escape (fromIntegral (fromEnum ch)) (urlEncodeBytes t)
  where
   escape b rs = '%' : showH (b `div` 16) (showH (b `mod` 16) rs)

   showH :: Word8 -> String -> String
   showH x xs
     | x <= 9    = to (o_0 + x) : xs
     | otherwise = to (o_A + (x-10)) : xs
    where
     to  = toEnum  .  fromIntegral
     fro = fromIntegral . fromEnum

     o_0 = fro '0'
     o_A = fro 'A'

getField :: BE.BEncode a => BE.BValue -> B.ByteString -> Either String a
getField (BE.BDict dict) f = searchDict dict >>= BE.fromBEncode
  where
    searchDict :: BE.BDict -> Either String BE.BValue
    searchDict BE.Nil = Left $ "Field " ++ BC.unpack f ++ " not in dict."
    searchDict (BE.Cons k v t)
      | k == f = return v
      | otherwise = searchDict t
getField _ _ = Left "Can't search field in a non-dictionary bencode value."

-- | Parse list of <4-byte ip address><2-byte port number>s.
--
-- >>> :{
--   execParser (B.pack [192, 168, 0, 1, 0x1b, 0x39,
--                       0, 0, 0, 0, 0x04, 0xd2]) readAddrs
-- :}
-- Right ([192.168.0.1:6969,0.0.0.0:1234],"")
--
readAddrs :: Parser [SockAddr]
readAddrs = do
    addr <- readAddr
    case addr of
      Nothing -> return []
      Just addr' -> (addr' :) <$> readAddrs
  where
    readAddr :: Parser (Maybe SockAddr)
    readAddr = tryP $ do
      ip <- readWord32LE
      port <- readWord16LE
      return $ SockAddrInet (PortNum port) ip
