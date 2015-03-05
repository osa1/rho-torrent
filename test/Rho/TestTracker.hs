-- | A very incomplete tracker implementation, to be used in tests.
module Rho.TestTracker where

import           Control.Applicative
import           Control.Concurrent
import           Control.Monad
import qualified Data.ByteString              as B
import qualified Data.ByteString.Builder      as BB
import qualified Data.ByteString.Lazy         as LB
import           Data.Monoid
import qualified Data.Set                     as S
import           Data.Word
import           Network.Socket               hiding (recvFrom, sendTo)
import           Network.Socket.ByteString    (recvFrom, sendTo)
import           System.Random

import           Rho.InfoHash
import           Rho.Parser
import           Rho.PeerComms.PeerId
import           Rho.TrackerComms.UDP.Request hiding (tid)
import           Rho.TrackerComms.UDP.Types

data TestTracker = TestTracker
  { connectedClients :: MVar (S.Set SockAddr)
  , trackerThread    :: ThreadId
  }

-- | Run the tracker using given UDP socket.
runTracker :: IO (TestTracker, SockAddr)
runTracker = do
    sock <- socket AF_INET Datagram defaultProtocol
    hostAddr <- inet_addr "127.0.0.1"
    bind sock (SockAddrInet aNY_PORT hostAddr)
    pn <- socketPort sock
    cs <- newMVar S.empty
    tid <- forkIO $ trackerLoop sock cs
    return (TestTracker cs tid, SockAddrInet pn hostAddr)

trackerLoop :: Socket -> MVar (S.Set SockAddr) -> IO ()
trackerLoop sock cs = forever $ do
    -- longest UDP message is announce request, and it's 98 bytes long.
    (msg, client@(SockAddrInet _ cIp)) <- recvFrom sock 98
    case parseRequest msg of
      Right (ConnectRequest tid) -> do
        cid <- randomIO
        void $ sendTo sock (mkConnectResponse cid tid) client
      Right (AnnounceRequest _ tid _ _ _ _ _ _ pn) -> do
        -- TODO: maybe handle close events here.
        -- TODO: we should be sending close events in the client.
        s' <- modifyMVar cs $ \s -> do
          let s' = S.insert (SockAddrInet pn cIp) s
          return (s', S.toList s')
        void $ sendTo sock (mkAnnounceResponse tid s') client
      Right ScrapeRequest{} -> putStrLn "unsupported msg: scrape"
      Left err -> putStrLn $ "Can't parse request: " ++ err

parseRequest :: B.ByteString -> Either String UDPRequest
parseRequest bs = fst <$> execParser bs requestParser
  where
    requestParser :: Parser UDPRequest
    requestParser = do
      connId <- readWord64
      if connId == 0x41727101980
        then do
          action <- readWord32
          if action == 0
            then ConnectRequest <$> readWord32
            else fail "wrong action for connect request"
        else do
          action <- readWord32
          if action == 1
            then annParser connId
            else fail "wrong action for announce request"

    annParser :: ConnectionId -> Parser UDPRequest
    annParser connId =
      AnnounceRequest connId
        <$> readWord32
        <*> (InfoHash . B.pack <$> replicateM 20 readWord)
        <*> (PeerId . B.pack <$> replicateM 20 readWord)
        <*> readWord64
        <*> readWord64
        <*> readWord64
        <*> (parseAnnEv =<< readWord32)
        <*> (PortNum <$> do
              void $ readWord32 -- skip ip
              void $ readWord32 -- skip key
              void $ readWord32 -- skip numwant
              readWord16LE)

    parseAnnEv :: Word32 -> Parser AnnounceEvent
    parseAnnEv 0 = return None
    parseAnnEv 1 = return Completed
    parseAnnEv 2 = return Started
    parseAnnEv 3 = return Stopped
    parseAnnEv i = fail $ "Invalid announce event: " ++ show i

mkConnectResponse :: ConnectionId -> TransactionId -> B.ByteString
mkConnectResponse cid tid = LB.toStrict . BB.toLazyByteString . mconcat $
    [ BB.word32BE 0
    , BB.word32BE tid
    , BB.word64BE cid
    ]

mkAnnounceResponse :: TransactionId -> [SockAddr] -> B.ByteString
mkAnnounceResponse tid socks = LB.toStrict . BB.toLazyByteString . mconcat $
    [ BB.word32BE 1
    , BB.word32BE tid
    , BB.word32BE (10 * 60) -- interval
    , BB.word32BE 0 -- leechers
    , BB.word32BE 0 -- seeders
    ] <> addrs
  where
    addrs = flip map socks $ \(SockAddrInet (PortNum w16) ip) ->
              BB.word32LE ip <> BB.word16LE w16
