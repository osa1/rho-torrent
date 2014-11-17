{-# LANGUAGE OverloadedStrings #-}

module Rho.PeerComms.Handshake where

import           Control.Monad
import           Data.Bits
import qualified Data.ByteString         as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8   as BC
import qualified Data.ByteString.Lazy    as LB
import           Data.Monoid

import           Rho.Parser

mkHandshake :: B.ByteString -> B.ByteString -> B.ByteString
mkHandshake infoHash peerId =
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

parseHandshake :: B.ByteString -> Either String (B.ByteString, B.ByteString, B.ByteString)
parseHandshake bs =
    case execParser bs handshakeParser of
      Just ((pstr, infoHash, peerId), rest) -> do
        assert ("Unknown pstr: " ++ BC.unpack pstr) (pstr == "BitTorrent protocol")
        assert ("info_hash length is wrong: " ++ show (B.length infoHash)) (B.length infoHash == 20)
        assert ("peer_id length is wrong: " ++ show (B.length peerId)) (B.length peerId == 20)
        return (infoHash, peerId, rest)
      Nothing -> Left "Can't parse handshake message."
  where
    assert :: String -> Bool -> Either String ()
    assert _   True  = Right ()
    assert err False = Left err

    handshakeParser :: Parser (B.ByteString, B.ByteString, B.ByteString)
    handshakeParser = do
      pstrLen <- readWord
      pstr <- replicateM (fromIntegral pstrLen) readWord
      _ <- replicateM 8 readWord
      infoHash <- replicateM 20 readWord
      peerId <- replicateM 20 readWord
      return (B.pack pstr, B.pack infoHash, B.pack peerId)
