{-# LANGUAGE OverloadedStrings #-}

module Rho.PeerComms.Handshake where

import           Data.Binary.Get
import           Data.Bits
import qualified Data.ByteString            as B
import qualified Data.ByteString.Builder    as BB
import qualified Data.ByteString.Char8      as BC
import qualified Data.ByteString.Lazy       as LB
import qualified Data.ByteString.Lazy.Char8 as LBC

import           Rho.InfoHash
import           Rho.PeerComms.PeerId

data ExtendedMsgSupport = Supports | DoesntSupport deriving (Show, Eq)

data Handshake = Handshake
  { hInfoHash  :: InfoHash
  , hPeerId    :: PeerId
  , hExtension :: ExtendedMsgSupport
  } deriving (Show, Eq)

mkHandshake :: InfoHash -> PeerId -> B.ByteString
mkHandshake (InfoHash infoHash) (PeerId peerId) =
    LB.toStrict . BB.toLazyByteString . mconcat $
      [ BB.word8 19 -- pstr len: standard for BitTorrent protocol
      , BB.byteString "BitTorrent protocol" -- pstr
      , BB.byteString $ B.pack
          [0, 0, 0, 0, 0,
           0 .|. 0x10, -- we support extension protocol
           0, 0]
      , BB.byteString infoHash
      , BB.byteString peerId
      ]

parseHandshake :: LB.ByteString -> Either String Handshake
parseHandshake bs =
    case runGetOrFail handshakeParser bs of
      Right (rest, _, (pstr, infoHash, peerId, extension)) -> do
        assert ("Unknown pstr: " ++ BC.unpack pstr) (pstr == "BitTorrent protocol")
        assert ("Unparsed handshake contents: " ++ LBC.unpack rest) (LB.null rest)
        return $ Handshake infoHash peerId extension
      Left (_, _, err) -> Left $ "Can't parse handshake message: " ++ err
  where
    assert :: String -> Bool -> Either String ()
    assert _   True  = Right ()
    assert err False = Left err

    handshakeParser :: Get (B.ByteString, InfoHash, PeerId, ExtendedMsgSupport)
    handshakeParser = do
      pstrLen <- getWord8
      pstr <- getByteString (fromIntegral pstrLen)
      reserveds <- getByteString 8
      let extension = if testBit (B.index reserveds 5) 4 then Supports else DoesntSupport
      infoHash <- getByteString 20
      peerId <- getByteString 20
      return (pstr, InfoHash infoHash, PeerId peerId, extension)
