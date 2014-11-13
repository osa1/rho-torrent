{-# LANGUAGE OverloadedStrings, RankNTypes, TupleSections #-}

-- | Communications with HTTP/UDP trackers.
module Rho.Comms where

import           Control.Applicative
import           Control.Concurrent.Async
import           Control.Concurrent.Chan
import           Control.Concurrent.MVar
import qualified Data.BEncode              as BE
import qualified Data.ByteString           as B
import qualified Data.ByteString.Builder   as BB
import qualified Data.ByteString.Char8     as BC
import qualified Data.ByteString.Lazy      as LB
import           Data.List                 (intercalate)
import qualified Data.Map                  as M
import           Data.Maybe
import           Data.Monoid
import           Data.Word
import           Network.Browser
import           Network.HTTP
import           Network.Socket            hiding (recv, recvFrom, send, sendTo)
import           Network.Socket.ByteString
import           Network.URI
import           System.Random             (randomIO)

import           Rho.Metainfo
import           Rho.Parser
import           Rho.Torrent
import           Rho.Utils

type PeerId        = B.ByteString -- ^ 20-byte

data PeerResponse = PeerResponse
  { prInterval      :: Word32
  , prLeechers      :: Maybe Word32
  , prSeeders       :: Maybe Word32
  , prPeers         :: [SockAddr]
  } deriving (Show)

type TrackerRespError = String


-- * Connections with HTTP trackers

peerRequestHTTP
  :: B.ByteString -> URI -> Torrent -> Metainfo -> IO (Either TrackerRespError PeerResponse)
peerRequestHTTP peerId uri torrent metainfo = do
    putStrLn $ "info_hash: " ++ show (B.length (iHash (mInfo metainfo)))
    (_, resp) <- browse $ do
      setAllowRedirects True -- handle HTTP redirects
      request $ defaultGETRequest $ updateURI uri
    return $ parseResp (BC.pack $ rspBody resp)
  where
    updateURI :: URI -> URI
    updateURI uri =
      let sepchar = if null (uriQuery uri) then '?' else '&' in
      uri{uriQuery = sepchar : intercalate "&" (map (\(k, v) -> k ++ '=' : v) args)}

    args :: [(String, String)]
    args =
      [ ("info_hash", urlEncodeBytes . iHash . mInfo $ metainfo)
      , ("peer_id", urlEncodeBytes peerId)
      , ("port", "5432") -- FIXME
      , ("uploaded", show $ uploaded torrent)
      , ("downloaded", show $ downloaded torrent)
      , ("left", show $ left torrent)
      , ("compact", "1")
      , ("numwant", "80")
      , ("event", "started")
      ]

    parseResp :: B.ByteString -> Either TrackerRespError PeerResponse
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
              peers <- maybe (Left "Can't parse peers.") (Right . fst) $
                         execParser peers_bs readAddrs
              return $ PeerResponse (fromMaybe interval minInterval)
                                    incomplete
                                    complete
                                    peers


-- * Connections with UDP trackers

type TransactionId = Word32
type ConnectionId  = Word64

type ConnectionCallback = ConnectionId -> IO ()

-- We need some state to manage UDP communications with trackers.
data UDPCommHandler = UDPCommHandler
  { sock         :: Socket
  , connCbs      :: MVar (M.Map TransactionId ConnectionCallback)
    -- ^ callbacks to call after successfully parsing ConnectionId
    -- from connect responses
  , announceVars :: MVar (M.Map TransactionId (MVar PeerResponse))
    -- ^ channels to send peer responses after successfully parsing
    -- announce responses
  }

initUDPCommHandler :: IO UDPCommHandler
initUDPCommHandler = do
    sock <- socket AF_INET Datagram defaultProtocol
    bind sock (SockAddrInet (fromIntegral (5432 :: Int)) 0)

    cbs <- newMVar M.empty
    peerRps <- newMVar M.empty

    -- data chan is used to push UDP packages to response handler
    dataChan <- spawnSockListener sock
    _ <- spawnResponseHandler dataChan cbs peerRps

    return $ UDPCommHandler sock cbs peerRps

-- | WARNING: This blocks. Use with `async`.
peerRequestUDP :: UDPCommHandler -> SockAddr -> PeerId -> Torrent -> IO PeerResponse
peerRequestUDP commHandler targetAddr peerId torrent = do
    var <- newEmptyMVar
    sendConnectReq (sock commHandler) targetAddr (connCbs commHandler) $
      sendAnnounceReq (sock commHandler) targetAddr peerId torrent (announceVars commHandler) var
    -- TODO: handle errrors (at least timeouts)
    takeMVar var

sendConnectReq
  :: Socket -> SockAddr
  -> MVar (M.Map TransactionId ConnectionCallback)
  -> ConnectionCallback
  -> IO ()
sendConnectReq sock targetAddr cbs cb = do
    transactionId <- randomIO
    -- set the callback before sending request, request may arrive too
    -- early (e.g. before setting the callback)
    modifyMVar_ cbs $ return . M.insert transactionId cb
    -- send the request
    let req = LB.toStrict . BB.toLazyByteString $
                BB.word64BE 0x41727101980 <> BB.word32BE 0 <> BB.word32BE transactionId
    sent <- sendTo sock req targetAddr
    -- TODO: at least add a debug print here to warn when not all bytes are sent
    return ()

sendAnnounceReq
  :: Socket -> SockAddr -> PeerId -> Torrent
  -> MVar (M.Map TransactionId (MVar PeerResponse))
  -> MVar PeerResponse
  -> ConnectionId -> IO ()
sendAnnounceReq skt trackerAddr peerId torrent peerRps var connId = do
    transactionId <- randomIO
    modifyMVar_ peerRps $ return . M.insert transactionId var
    let req = LB.toStrict . BB.toLazyByteString . mconcat $
                [ BB.word64BE connId
                , BB.word32BE 1 -- action: announce
                , BB.word32BE transactionId
                , BB.byteString (infoHash torrent)
                , BB.byteString peerId
                , BB.word64BE (downloaded torrent)
                , BB.word64BE (left torrent)
                , BB.word64BE (uploaded torrent)
                , BB.word32BE 2 -- event: started
                , BB.word32BE 0 -- IP address
                , BB.word32BE 0 -- key
                , BB.word32BE (-1) -- numwant
                , BB.word16BE 5432 -- port FIXME
                ]
    sent <- sendTo skt req trackerAddr
    -- TODO: maybe check if sent == length of msg
    return ()

-- | Socket listener reads stream from the socket and passes it to channel.
spawnSockListener :: Socket -> IO (Chan (B.ByteString, SockAddr))
spawnSockListener skt = do
    dataChan <- newChan
    _ <- async (sockListener dataChan)
    return dataChan
  where
    -- | As far as I can see there's no way to handle `recvFrom` timeout
    -- exceptions. Instead of hacking my way out, I decided to go with this:
    -- This worker shouldn't be bothered with any events -- it just read
    -- from the socket and pass the data. It only fails and terminates when
    -- socket closed/main thread terminated.
    sockListener :: Chan (B.ByteString, SockAddr) -> IO ()
    sockListener dataChan = do
      (contents, src) <- recvFrom skt msg_size
      putStrLn $ "Got " ++ show (B.length contents) ++ " bytes from: " ++ show src
      writeChan dataChan (contents, src)
      sockListener dataChan

    -- | Can't see anyting relevant in specs, but while testing I realized
    -- that trackers are always returning 1200 bytes. Using slightly bigger
    -- number here.
    msg_size = 1500

spawnResponseHandler
  :: Chan (B.ByteString, SockAddr)
  -> MVar (M.Map TransactionId ConnectionCallback)
  -> MVar (M.Map TransactionId (MVar PeerResponse))
  -> IO ()
spawnResponseHandler dataChan cbs peerRps = do
    _ <- async responseHandler
    return ()
  where
    responseHandler :: IO ()
    responseHandler = do
        (resp, src) <- readChan dataChan
        putStrLn $ "Got response from " ++ show src
        case execParser resp readWord32 of
          Just (0, resp') -> handleConnectResp resp' cbs
          Just (1, resp') -> handleAnnounceResp resp' peerRps
          Just (2, resp') -> handleScrapeResp resp'
          Just (3, resp') -> handleErrorResp resp' cbs
          Just (n, _) -> putStrLn $ "Unknown response: " ++ show n
          Nothing -> putStrLn $ "Got ill-formed response"
        responseHandler

handleConnectResp :: B.ByteString -> MVar (M.Map TransactionId ConnectionCallback) -> IO ()
handleConnectResp bs cbs = do
    case parseResp of
      Nothing -> putStrLn "Can't parse connect response."
      Just (tid, cid) -> do
        putStrLn $ "Connection id for transaction id " ++ show tid ++ ": " ++ show cid
        cbMap <- readMVar cbs
        case M.lookup tid cbMap of
          Nothing -> putStrLn "Can't find tid in callback map. Ignoring response."
          Just cb -> do
            putStrLn "Found a callback. Running..."
            cb cid
  where
    parseResp :: Maybe (TransactionId, ConnectionId)
    parseResp = fmap fst . execParser bs $ do
      tid <- readWord32
      cid <- readWord64
      return (tid, cid)

handleAnnounceResp :: B.ByteString -> MVar (M.Map TransactionId (MVar PeerResponse)) -> IO ()
handleAnnounceResp bs peerRps = do
    putStrLn "handling announce response"
    case parseResp of
      Nothing -> putStrLn "Can't parse announce response."
      Just (tid, ret) -> do
        respVar <- modifyMVar peerRps $
          \m -> return (M.delete tid m, M.lookup tid m)
        case respVar of
          Nothing -> putStrLn "Got unexpected announce response."
          Just respVar' -> putMVar respVar' ret
  where
    parseResp :: Maybe (TransactionId, PeerResponse)
    parseResp = fmap fst . execParser bs $ do
      tid <- readWord32
      interval <- readWord32
      leechers <- readWord32
      seeders <- readWord32
      addrs <- readAddrs
      return (tid, PeerResponse interval (Just leechers) (Just seeders) addrs)

readAddrs :: Parser [SockAddr]
readAddrs = do
    addr <- readAddr
    case addr of
      Nothing -> return []
      Just addr' -> (addr' :) <$> readAddrs
  where
    readAddr :: Parser (Maybe SockAddr)
    readAddr = tryP $ do
      ip <- readWord32LE
      port <- readWord16LE
      return $ SockAddrInet (PortNum port) ip

handleScrapeResp :: B.ByteString -> IO ()
handleScrapeResp _ = putStrLn "handling scrape response"

handleErrorResp :: B.ByteString -> MVar (M.Map TransactionId ConnectionCallback) -> IO ()
handleErrorResp bs cbs = do
    case parseResp of
      Nothing -> putStrLn "Can't parse error response"
      Just (tid, msg) -> do
        putStrLn $ "Error response for " ++ show tid ++ ": " ++ msg
        putStrLn $ "Removing callbacks for " ++ show tid
        modifyMVar_ cbs $ return . M.delete tid
  where
    parseResp :: Maybe (TransactionId, String)
    parseResp = fmap fst . execParser bs $ do
      transactionId <- readWord32
      msg <- consume
      return (transactionId, BC.unpack msg)
