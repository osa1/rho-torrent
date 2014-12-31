{-# LANGUAGE OverloadedStrings #-}

-- | Connections with HTTP trackers.
module Rho.TrackerComms.HTTP where

import qualified Data.BEncode                  as BE
import qualified Data.ByteString               as B
import qualified Data.ByteString.Char8         as BC
import           Data.List                     (intercalate)
import           Data.Maybe                    (fromMaybe)
import           Data.Word
import           Network.Browser
import           Network.HTTP                  (getRequest, rspBody)
import           Network.Socket                (PortNumber)
import           Network.URI                   (URI, uriQuery)

import           Rho.InfoHash
import           Rho.Parser
import           Rho.PeerComms.PeerId
import           Rho.TrackerComms.PeerResponse
import           Rho.Utils

peerRequestHTTP
  :: PeerId -> PortNumber -> URI -> (Word64, Word64, Word64)
  -> InfoHash -> IO (Either String PeerResponse)
peerRequestHTTP (PeerId peerId) port uri (downloaded, left, uploaded) hash = do
    (_, resp) <- browse $ do
      setOutHandler (const $ return ())
      setAllowRedirects True -- handle HTTP redirects
      request $ getRequest (show updatedURI)
    return $ parseResp (BC.pack $ rspBody resp)
  where
    updatedURI =
      let sepchar = if null (uriQuery uri) then '?' else '&' in
      uri{uriQuery = sepchar : intercalate "&" (map (\(k, v) -> k ++ '=' : v) args)}

    args :: [(String, String)]
    args =
      [ ("info_hash", urlEncodeBytes $ unwrapInfoHash hash)
      , ("peer_id", urlEncodeBytes peerId)
      , ("port", show port)
      , ("uploaded", show uploaded)
      , ("downloaded", show downloaded)
      , ("left", show left)
      , ("compact", "1")
      , ("numwant", "80")
      , ("event", "started")
      ]

    parseResp :: B.ByteString -> Either String PeerResponse
    parseResp rsp = do
      bv <- BE.decode rsp
      case getField bv "failure reason" of
        Right reason -> Left (BC.unpack reason)
        Left _ -> do
          interval <- getField bv "interval"
          let minInterval = opt $ getField bv "min interval"
              _trackerId = opt $ getField bv "tracker id" :: Maybe B.ByteString
              complete = opt $ getField bv "complete"
              incomplete = opt $ getField bv "incomplete"
          -- TOOD: peers_bs is either a dictionary or a byte string
          -- (in case of compact form). currently only compact form
          -- is handled.
          peers_bs <- getField bv "peers"
          peers <- fst `fmap` execParser peers_bs readAddrs
          return $ PeerResponse (fromMaybe interval minInterval)
                                incomplete
                                complete
                                peers

    opt :: Either a b -> Maybe b
    opt (Right ret) = Just ret
    opt (Left _) = Nothing
