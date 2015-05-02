-- | ... because every project needs one.
module Rho.Utils where

import qualified Data.Attoparsec.ByteString.Char8 as P
import qualified Data.BEncode                     as BE
import qualified Data.BEncode.BDict               as BE
import qualified Data.BEncode.Internal            as BEI
import qualified Data.BEncode.Types               as BE
import           Data.Binary.Get
import           Data.Bits                        (shiftL)
import qualified Data.ByteString                  as B
import qualified Data.ByteString.Char8            as BC
import           Data.ByteString.Internal         (ByteString (PS))
import qualified Data.ByteString.Lazy             as LB
import           Data.Char
import           Data.IORef                       (IORef, atomicModifyIORef')
import           Data.Vector.Storable             (Vector, unsafeFromForeignPtr,
                                                   unsafeToForeignPtr)
import           Data.Word
import           Network.Socket                   (PortNumber (..),
                                                   SockAddr (..))
import           System.Clock

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
--   runGet readAddrs (LB.pack [192, 168, 0, 1, 0x1b, 0x39,
--                              0, 0, 0, 0, 0x04, 0xd2])
-- :}
-- [192.168.0.1:6969,0.0.0.0:1234]
--
readAddrs :: Get [SockAddr]
readAddrs = do
    emp <- isEmpty
    if emp
      then return []
      else do
        ip <- getWord32le
        port <- getWord16le
        (SockAddrInet (PortNum port) ip :) <$> readAddrs

getResult
  :: Either (LB.ByteString, ByteOffset, String) (LB.ByteString, ByteOffset, a)
  -> Either String a
getResult (Right (_, _, ret)) = Right ret
getResult (Left  (_, _, err)) = Left err

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

-- | Return system time in milliseconds.
currentTimeMillis :: IO Int
currentTimeMillis = do
    TimeSpec s ns <- getTime Monotonic
    return . fromIntegral $ (s * 1000) + (ns `div` 1000000)

-- | Like `atomicModifyIORef'`, but returns ().
atomicModifyIORef_ :: IORef a -> (a -> a) -> IO ()
atomicModifyIORef_ ref f = atomicModifyIORef' ref $ \v -> (f v, ())
{-# INLINE atomicModifyIORef_ #-}

-- | Convert a ByteString into a storable Vector.
bsToByteVector :: ByteString -> Vector Word8
bsToByteVector (PS fptr offset idx) = unsafeFromForeignPtr fptr offset idx

-- | Convert a storable Vector to a ByteString.
bsFromByteVector :: Vector Word8 -> ByteString
bsFromByteVector v =
    PS fptr offset idx
  where
    (fptr, offset, idx) = unsafeToForeignPtr v

-- | Time difference.
--
-- >>> TimeSpec 1 0 `dt` TimeSpec 0 500000000
-- TimeSpec {sec = 0, nsec = 500000000}
--
-- Make sure that first argument is bigger than second one.
--
dt :: TimeSpec -> TimeSpec -> TimeSpec
dt (TimeSpec s1 ns1) ts2@(TimeSpec s2 ns2)
  | ns1 < ns2 = dt (TimeSpec (s1 - 1) (ns1 + 10^9)) ts2
  | otherwise = TimeSpec (s1 - s2) (ns1 - ns2)

-- | Convert a timespec to seconds, ignoring nanoseconds part.
tsToSec :: TimeSpec -> Int
tsToSec (TimeSpec s _) = fromIntegral s
