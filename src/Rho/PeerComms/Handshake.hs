{-# LANGUAGE OverloadedStrings #-}

module Rho.PeerComms.Handshake where

import           Control.Monad
import           Data.Bits
import qualified Data.ByteString         as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8   as BC
import qualified Data.ByteString.Lazy    as LB
import           Data.Monoid

import           Rho.InfoHash
import           Rho.Parser

-- | 20-byte peer_id
newtype PeerId = PeerId B.ByteString deriving (Show, Eq)

mkHandshake :: InfoHash -> PeerId -> B.ByteString
mkHandshake (InfoHash infoHash) (PeerId peerId) =
    LB.toStrict . BB.toLazyByteString . mconcat $
      [ BB.word8 19 -- pstr len: standard for BitTorrent protocol
      , BB.byteString "BitTorrent protocol" -- pstr
      , BB.byteString $ B.pack
          [0, 0, 0, 0, 0,
           0 .&. 0x10, -- we support extension protocol
           0, 0]
      , BB.byteString infoHash
      , BB.byteString peerId
      ]

parseHandshake :: B.ByteString -> Either String (InfoHash, PeerId, B.ByteString)
parseHandshake bs =
    case execParser bs handshakeParser of
      Right ((pstr, infoHash, peerId), rest) -> do
        assert ("Unknown pstr: " ++ BC.unpack pstr) (pstr == "BitTorrent protocol")
        return (infoHash, peerId, rest)
      Left err -> Left $ "Can't parse handshake message: " ++ err
  where
    assert :: String -> Bool -> Either String ()
    assert _   True  = Right ()
    assert err False = Left err

    handshakeParser :: Parser (B.ByteString, InfoHash, PeerId)
    handshakeParser = do
      pstrLen <- readWord
      pstr <- replicateM (fromIntegral pstrLen) readWord
      _ <- replicateM 8 readWord
      infoHash <- replicateM 20 readWord
      peerId <- replicateM 20 readWord
      return (B.pack pstr, InfoHash $ B.pack infoHash, PeerId $ B.pack peerId)
