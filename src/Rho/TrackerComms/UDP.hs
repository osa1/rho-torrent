{-# LANGUAGE OverloadedStrings #-}

-- | Connections with UDP trackers
module Rho.TrackerComms.UDP where

import           Control.Concurrent.Async
import           Control.Concurrent.Chan
import           Control.Concurrent.MVar
import qualified Data.ByteString               as B
import qualified Data.Map                      as M
import           Data.Word
import           Network.Socket                hiding (recv, recvFrom, send,
                                                sendTo)
import           Network.Socket.ByteString
import           System.Random                 (randomIO)

import           Rho.PeerComms.Handshake
import           Rho.Torrent
import           Rho.TrackerComms.PeerResponse
import           Rho.TrackerComms.UDP.Request
import           Rho.TrackerComms.UDP.Response
import           Rho.TrackerComms.UDP.Types

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
    let req = mkTrackerMsg $ ConnectRequest transactionId
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
    let req = mkTrackerMsg $ AnnounceRequest connId transactionId (infoHash torrent)
                                             peerId (downloaded torrent) (left torrent)
                                             (uploaded torrent) Started
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
        case parseUDPResponse resp of
          Right (ConnectResponse tid cid) -> handleConnectResp tid cid cbs
          Right (AnnounceResponse tid ps) -> handleAnnounceResp tid ps peerRps
          Right (ScrapeResponse tid ps) -> handleScrapeResp tid ps
          Right (ErrorResponse tid err) -> handleErrorResp tid err cbs
          Left err -> putStrLn $ "Can't parse server response: " ++ err
        responseHandler

handleConnectResp
  :: TransactionId -> ConnectionId -> MVar (M.Map TransactionId ConnectionCallback) -> IO ()
handleConnectResp tid cid cbs = do
    putStrLn $ "Connection id for transaction id " ++ show tid ++ ": " ++ show cid
    cbMap <- readMVar cbs
    case M.lookup tid cbMap of
      Nothing -> putStrLn "Can't find tid in callback map. Ignoring response."
      Just cb -> do
        putStrLn "Found a callback. Running..."
        cb cid

handleAnnounceResp
  :: TransactionId -> PeerResponse -> MVar (M.Map TransactionId (MVar PeerResponse)) -> IO ()
handleAnnounceResp tid ret peerRps = do
    putStrLn "handling announce response"
    respVar <- modifyMVar peerRps $
      \m -> return (M.delete tid m, M.lookup tid m)
    case respVar of
      Nothing -> putStrLn "Got unexpected announce response."
      Just respVar' -> putMVar respVar' ret

handleScrapeResp :: TransactionId -> [(Word32, Word32, Word32)] -> IO ()
handleScrapeResp _ _ = putStrLn "handling scrape response"

handleErrorResp :: TransactionId -> String -> MVar (M.Map TransactionId ConnectionCallback) -> IO ()
handleErrorResp tid msg cbs = do
    putStrLn $ "Error response for " ++ show tid ++ ": " ++ msg
    putStrLn $ "Removing callbacks for " ++ show tid
    modifyMVar_ cbs $ return . M.delete tid
