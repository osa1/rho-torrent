-- | ... because every project needs one.
module Rho.Utils where

import           Control.Monad
import qualified Data.BEncode          as BE
import qualified Data.BEncode.BDict    as BE
import qualified Data.BEncode.Types    as BE
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BC
import           Data.Char
import           Data.Word

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
getField (BE.BDict dict) f = join $ BE.fromBEncode `fmap` searchDict dict
  where
    searchDict :: BE.BDict -> Either String BE.BValue
    searchDict BE.Nil = Left $ "Field " ++ BC.unpack f ++ " not in dict."
    searchDict (BE.Cons k v t)
      | k == f = return v
      | otherwise = searchDict t
getField _ _ = Left "Can't search field in a non-dictionary bencode value."

opt :: Either a b -> Maybe b
opt (Right ret) = Just ret
opt (Left _) = Nothing