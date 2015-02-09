-- | Responses from UDP trackers.
module Rho.TrackerComms.UDP.Response where

import           Control.Applicative
import qualified Data.ByteString               as B
import qualified Data.ByteString.Char8         as BC
import           Data.Word

import           Rho.Parser
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
parseUDPResponse bs = fmap fst . execParser bs $ do
    w <- readWord32
    case w of
      0 -> uncurry ConnectResponse <$> parseConnectResp
      1 -> uncurry AnnounceResponse <$> parseAnnounceResp
      2 -> uncurry ScrapeResponse <$> parseScrapeResp
      3 -> uncurry ErrorResponse <$> parseErrorResp
      n -> fail $ "Unknown response: " ++ show n

parseConnectResp :: Parser (TransactionId, ConnectionId)
parseConnectResp = do
    tid' <- readWord32
    cid  <- readWord64
    return (tid', cid)

parseAnnounceResp :: Parser (TransactionId, PeerResponse)
parseAnnounceResp = do
    tid' <- readWord32
    interval <- readWord32
    leechers <- readWord32
    seeders <- readWord32
    addrs <- readAddrs
    return (tid', PeerResponse interval (Just leechers) (Just seeders) addrs)

parseScrapeResp :: Parser (TransactionId, [(Word32, Word32, Word32)])
parseScrapeResp = do
    tid' <- readWord32
    lst  <- parseList
    return (tid', lst)
  where
    parseList = do
      e <- tryP ((,,) <$> readWord32 <*> readWord32 <*> readWord32)
      case e of
        Nothing -> return []
        Just e' -> (:) e' <$> parseList

parseErrorResp :: Parser (TransactionId, String)
parseErrorResp = do
    tid' <- readWord32
    msg  <- consume
    return (tid', BC.unpack msg)
