-- | Parsing big-endian words from ByteString.
module Rho.Parser where

import           Control.Applicative
import           Data.Bits
import qualified Data.ByteString     as B
import           Data.Word

newtype Parser a = Parser { runParser :: B.ByteString -> Either String (a, B.ByteString) }

instance Functor Parser where
    fmap f (Parser p) = Parser $ \bs -> do
      (v, bs') <- p bs
      return (f v, bs')

instance Applicative Parser where
    pure v = Parser $ \bs -> return (v, bs)
    Parser fn <*> Parser v = Parser $ \bs -> do
      (fn', bs') <- fn bs
      (v', bs'') <- v bs'
      return (fn' v', bs'')

instance Monad Parser where
    return = pure
    Parser p >>= f = Parser $ \bs -> do
      (v, bs') <- p bs
      runParser (f v) bs'
    fail err = Parser $ \bs -> Left err

execParser :: B.ByteString -> Parser a -> Either String (a, B.ByteString)
execParser bs (Parser f) = f bs

tryP :: Parser a -> Parser (Maybe a)
tryP (Parser p) = Parser $ \bs ->
    case p bs of
      Left _ -> Right (Nothing, bs)
      Right (p', bs') -> Right (Just p', bs')

-- | Return rest of the input. Parsing will fail after this step.
--
-- >>> execParser (B.pack [45, 45, 45, 45]) consume
-- Right ("----","")
--
consume :: Parser B.ByteString
consume = Parser $ \bs -> return (bs, B.empty)

-- | Try to read Word32.
--
-- >>> :{
--   execParser (B.pack [1, 2, 3, 4])
--     ((,,,) <$> readWord <*> readWord <*> readWord <*> readWord)
-- :}
-- Right ((1,2,3,4),"")
--
readWord :: Parser Word8
readWord = Parser $ \bs ->
    case B.uncons bs of
      Nothing        -> Left "Can't read word: end of string."
      Just (b, rest) -> return (b, rest)

-- | Try to read little-endian Word16 from big-endian byte string.
--
-- >>> execParser (B.pack [1, 0]) readWord16LE
-- Right (1,"")
--
readWord16LE :: Parser Word16
readWord16LE = do
    w1 <- readWord
    w2 <- readWord
    return $ fromIntegral w2 `shiftL` 8 + fromIntegral w1

-- | Try to read Word32 from big-endian byte string.
--
-- >>> execParser (B.pack [1, 2, 3, 4]) readWord32
-- Right (16909060,"")
--
readWord32 :: Parser Word32
readWord32 = do
    w1 <- readWord
    w2 <- readWord
    w3 <- readWord
    w4 <- readWord
    return $    fromIntegral w1 `shiftL` 24
              + fromIntegral w2 `shiftL` 16
              + fromIntegral w3 `shiftL` 8
              + fromIntegral w4

-- | Try to read little-endian Word32 from big-endian byte string.
readWord32LE :: Parser Word32
readWord32LE = do
    w1 <- readWord
    w2 <- readWord
    w3 <- readWord
    w4 <- readWord
    return $    fromIntegral w4 `shiftL` 24
              + fromIntegral w3 `shiftL` 16
              + fromIntegral w2 `shiftL` 8
              + fromIntegral w1

-- | Try to read Word64 from big-endian byte string.
--
-- >>> execParser (B.pack [1, 2, 3, 4, 5, 6, 7, 8, 45]) readWord64
-- Right (72623859790382856,"-")
--
readWord64 :: Parser Word64
readWord64 = do
    w1 <- readWord32
    w2 <- readWord32
    return $ fromIntegral w1 `shiftL` 32 + fromIntegral w2
