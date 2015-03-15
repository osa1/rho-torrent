-- | Responses from UDP trackers.
module Rho.TrackerComms.UDP.Response where

import           Control.Applicative
import           Data.Binary.Get
import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as LB
import qualified Data.ByteString.Lazy.Char8    as LBC
import           Data.Word

import           Rho.TrackerComms.PeerResponse
import           Rho.TrackerComms.UDP.Types
import           Rho.Utils

data UDPResponse
  = ConnectResponse TransactionId ConnectionId
  | AnnounceResponse TransactionId PeerResponse
  | ScrapeResponse
      TransactionId
      -- (seeders, completed, leechers)
      [(Word32, Word32, Word32)]
  | ErrorResponse TransactionId String
  deriving (Show, Eq)

tid :: UDPResponse -> TransactionId
tid (ConnectResponse tid' _)  = tid'
tid (AnnounceResponse tid' _) = tid'
tid (ScrapeResponse tid' _)   = tid'
tid (ErrorResponse tid' _)    = tid'

parseUDPResponse :: B.ByteString -> Either String UDPResponse
parseUDPResponse bs = getResult $ flip runGetOrFail (LB.fromStrict bs) $ do
    w <- getWord32be
    case w of
      0 -> uncurry ConnectResponse <$> parseConnectResp
      1 -> uncurry AnnounceResponse <$> parseAnnounceResp
      2 -> uncurry ScrapeResponse <$> parseScrapeResp
      3 -> uncurry ErrorResponse <$> parseErrorResp
      n -> fail $ "Unknown response: " ++ show n

parseConnectResp :: Get (TransactionId, ConnectionId)
parseConnectResp = do
    tid' <- getWord32be
    cid  <- getWord64be
    return (tid', cid)

parseAnnounceResp :: Get (TransactionId, PeerResponse)
parseAnnounceResp = do
    tid' <- getWord32be
    interval <- getWord32be
    leechers <- getWord32be
    seeders <- getWord32be
    addrs <- readAddrs
    return (tid', PeerResponse interval (Just leechers) (Just seeders) addrs)

parseScrapeResp :: Get (TransactionId, [(Word32, Word32, Word32)])
parseScrapeResp = do
    tid' <- getWord32be
    lst  <- parseList
    return (tid', lst)
  where
    parseList = do
      emp <- isEmpty
      if emp
        then return []
        else do
          e <- ((,,) <$> getWord32be <*> getWord32be <*> getWord32be)
          (e :) <$> parseList

parseErrorResp :: Get (TransactionId, String)
parseErrorResp = do
    tid' <- getWord32be
    msg  <- getRemainingLazyByteString
    return (tid', LBC.unpack msg)
