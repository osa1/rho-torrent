{-# LANGUAGE OverloadedStrings #-}

-- | Connections with HTTP trackers.
module Rho.TrackerComms.HTTP where

import qualified Data.BEncode                  as BE
import qualified Data.ByteString               as B
import qualified Data.ByteString.Char8         as BC
import           Data.List                     (intercalate)
import           Data.Maybe                    (fromMaybe)
import           Network.Browser               (browse, request,
                                                setAllowRedirects)
import           Network.HTTP                  (defaultGETRequest, rspBody)
import           Network.URI                   (URI, uriQuery)

import           Rho.InfoHash
import           Rho.Metainfo
import           Rho.Parser
import           Rho.PeerComms.Handshake
import           Rho.Torrent
import           Rho.TrackerComms.PeerResponse
import           Rho.Utils

peerRequestHTTP
  :: PeerId -> URI -> Torrent -> Metainfo -> IO (Either String PeerResponse)
peerRequestHTTP (PeerId peerId) uri torrent metainfo = do
    putStrLn $ "info_hash: " ++ show (B.length (unwrapInfoHash (iHash (mInfo metainfo))))
    (_, resp) <- browse $ do
      setAllowRedirects True -- handle HTTP redirects
      request $ defaultGETRequest updatedURI
    return $ parseResp (BC.pack $ rspBody resp)
  where
    updatedURI =
      let sepchar = if null (uriQuery uri) then '?' else '&' in
      uri{uriQuery = sepchar : intercalate "&" (map (\(k, v) -> k ++ '=' : v) args)}

    args :: [(String, String)]
    args =
      [ ("info_hash", urlEncodeBytes . unwrapInfoHash . iHash . mInfo $ metainfo)
      , ("peer_id", urlEncodeBytes peerId)
      , ("port", "5432") -- FIXME
      , ("uploaded", show $ uploaded torrent)
      , ("downloaded", show $ downloaded torrent)
      , ("left", show $ left torrent)
      , ("compact", "1")
      , ("numwant", "80")
      , ("event", "started")
      ]

    parseResp :: B.ByteString -> Either String PeerResponse
    parseResp rsp =
      case BE.decode rsp of
        Left err -> Left err
        Right bv ->
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
              peers <- fmap fst $ execParser peers_bs readAddrs
              return $ PeerResponse (fromMaybe interval minInterval)
                                    incomplete
                                    complete
                                    peers

    opt :: Either a b -> Maybe b
    opt (Right ret) = Just ret
    opt (Left _) = Nothing
