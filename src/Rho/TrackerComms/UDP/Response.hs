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
tid (ConnectResponse tid _)  = tid
tid (AnnounceResponse tid _) = tid
tid (ScrapeResponse tid _)   = tid
tid (ErrorResponse tid _)    = tid

parseUDPResponse :: B.ByteString -> Either String UDPResponse
parseUDPResponse bs =
    case execParser bs readWord32 of
      Right (0, rest) -> uncurry ConnectResponse <$> parseConnectResp rest
      Right (1, rest) -> uncurry AnnounceResponse <$> parseAnnounceResp rest
      Right (2, rest) -> uncurry ScrapeResponse <$> parseScrapeResp rest
      Right (3, rest) -> uncurry ErrorResponse <$> parseErrorResp rest
      Right (n, _)    -> fail $ "Unknown response: " ++ show n
      Left err        -> fail $ "Got ill-formed response: " ++ err

parseConnectResp :: B.ByteString -> Either String (TransactionId, ConnectionId)
parseConnectResp bs = fmap fst . execParser bs $ do
    tid <- readWord32
    cid <- readWord64
    return (tid, cid)

parseAnnounceResp :: B.ByteString -> Either String (TransactionId, PeerResponse)
parseAnnounceResp bs = fmap fst . execParser bs $ do
    tid <- readWord32
    interval <- readWord32
    leechers <- readWord32
    seeders <- readWord32
    addrs <- readAddrs
    return (tid, PeerResponse interval (Just leechers) (Just seeders) addrs)

parseScrapeResp :: B.ByteString -> Either String (TransactionId, [(Word32, Word32, Word32)])
parseScrapeResp bs = fmap fst . execParser bs $ do
    tid <- readWord32
    lst <- parseList
    return (tid, lst)
  where
    parseList = do
      e <- tryP ((,,) <$> readWord32 <*> readWord32 <*> readWord32)
      case e of
        Nothing -> return []
        Just e' -> (:) e' <$> parseList

parseErrorResp :: B.ByteString -> Either String (TransactionId, String)
parseErrorResp bs = fmap fst . execParser bs $ do
    transactionId <- readWord32
    msg <- consume
    return (transactionId, BC.unpack msg)
