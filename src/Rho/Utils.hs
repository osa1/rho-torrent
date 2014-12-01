-- | ... because every project needs one.
module Rho.Utils where

import           Control.Applicative
import qualified Data.Attoparsec.ByteString.Char8 as P
import qualified Data.BEncode                     as BE
import qualified Data.BEncode.BDict               as BE
import qualified Data.BEncode.Internal            as BEI
import qualified Data.BEncode.Types               as BE
import           Data.Bits                        (shiftL)
import qualified Data.ByteString                  as B
import qualified Data.ByteString.Char8            as BC
import           Data.Char
import           Data.Word
import           Network.Socket                   (PortNumber (..),
                                                   SockAddr (..))

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

-- | Search a key in BEncode value(BValue). BValue should be a BDict.
--
-- >>> :m + Data.BEncode
-- >>> :{
--   decode (BC.pack "d8:msg_typei0e5:piecei0ee") >>=
--     \bv -> getField bv (BC.pack "piece") :: Either String Integer
-- :}
-- Right 0
--
getField :: BE.BEncode a => BE.BValue -> B.ByteString -> Either String a
getField (BE.BDict dict) f = searchDict dict >>= BE.fromBEncode
  where
    searchDict :: BE.BDict -> Either String BE.BValue
    searchDict BE.Nil = Left $ "Field " ++ BC.unpack f ++ " not in dict."
    searchDict (BE.Cons k v t)
      | k == f = return v
      | otherwise = searchDict t
getField _ _ = Left "Can't search field in a non-dictionary bencode value."

-- | Similar to `BE.decode`, but returns non-consumed ByteString when
-- succeeds.
--
-- This is necessary when parsing BEP9 data message, we need to consume
-- rest of the message after parsing bencode part.
--
-- >>> :{
--   decodeNonConsumed (BC.pack "d8:msg_typei0e5:piecei0eeextra_data")
--     :: BE.Result (BE.BValue, BC.ByteString)
-- :}
-- Right (BDict (Cons "msg_type" (BInteger 0) (Cons "piece" (BInteger 0) Nil)),"extra_data")
--
decodeNonConsumed :: BE.BEncode a => B.ByteString -> Either String (a, B.ByteString)
decodeNonConsumed bs =
    case P.parse BEI.parser bs of
      P.Fail _ _ err -> Left err
      P.Done rest result -> BE.fromBEncode result >>= \b -> Right (b, rest)
      P.Partial cont ->
        case cont B.empty of
          P.Fail _ _ err -> Left err
          P.Done rest result -> BE.fromBEncode result >>= \b -> Right (b, rest)
          P.Partial _ -> error "decodeNonConsumed: impossible error!"

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

-- | Make a `Word16` from bytes. First argument is most-significant byte.
--
-- >>> mkWord16 0x1 0x0
-- 256
--
-- >>> mkWord16 0x0 0x1
-- 1
--
mkWord16 :: Word8 -> Word8 -> Word16
mkWord16 w1 w2 =
    fromIntegral w1 `shiftL` 8
  + fromIntegral w2
{-# INLINE mkWord16 #-}

-- | Make a `Word32` from bytes. First argument is most-significant byte.
--
-- >>> mkWord32 0x1 0x0 0x0 0x0
-- 16777216
--
-- >>> mkWord32 0x0 0x0 0x0 0x1
-- 1
--
mkWord32 :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
mkWord32 w1 w2 w3 w4 =
    fromIntegral w1 `shiftL` 24
  + fromIntegral w2 `shiftL` 16
  + fromIntegral w3 `shiftL` 8
  + fromIntegral w4
{-# INLINE mkWord32 #-}

-- | Make a `Word64` from bytes. First argument is most-significant byte.
--
-- >>> mkWord64 0x1 0x0 0x0 0x0 0x0 0x0 0x0 0x0
-- 72057594037927936
--
-- >>> mkWord64 0x0 0x0 0x0 0x0 0x0 0x0 0x0 0x1
-- 1
--
mkWord64 :: Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word64
mkWord64 w1 w2 w3 w4 w5 w6 w7 w8 =
    fromIntegral (mkWord32 w1 w2 w3 w4) `shiftL` 32
  + fromIntegral (mkWord32 w5 w6 w7 w8)
{-# INLINE mkWord64 #-}
