-- | A very incomplete tracker implementation, to be used in tests.
module Rho.TestTracker where

import           Control.Applicative
import           Control.Concurrent
import           Control.Monad
import           Data.Binary.Get
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
import           Rho.PeerComms.PeerId
import           Rho.TrackerComms.UDP.Request hiding (tid)
import           Rho.TrackerComms.UDP.Types
import           Rho.Utils

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
parseRequest bs = getResult $ runGetOrFail requestParser (LB.fromStrict bs)
  where
    requestParser :: Get UDPRequest
    requestParser = do
      connId <- getWord64be
      if connId == 0x41727101980
        then do
          action <- getWord32be
          if action == 0
            then ConnectRequest <$> getWord32be
            else fail "wrong action for connect request"
        else do
          action <- getWord32be
          if action == 1
            then annParser connId
            else fail "wrong action for announce request"

    annParser :: ConnectionId -> Get UDPRequest
    annParser connId =
      AnnounceRequest connId
        <$> getWord32be
        <*> (InfoHash <$> getByteString 20)
        <*> (PeerId <$> getByteString 20)
        <*> getWord64be
        <*> getWord64be
        <*> getWord64be
        <*> (parseAnnEv =<< getWord32be)
        <*> (PortNum <$> do
              void $ getWord32be -- skip ip
              void $ getWord32be -- skip key
              void $ getWord32be -- skip numwant
              getWord16le)

    parseAnnEv :: Word32 -> Get AnnounceEvent
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
