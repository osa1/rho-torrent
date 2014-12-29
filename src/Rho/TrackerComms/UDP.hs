{-# LANGUAGE OverloadedStrings #-}

-- | Connections with UDP trackers
--
-- As a convention, all functions block except for the ones that return
-- `Async a`.
--
-- TODO: We get a new connection_id for each request for now.
--
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

import           Rho.InfoHash
import           Rho.PeerComms.Handshake
import           Rho.SessionState              (SessStats)
import           Rho.TrackerComms.PeerResponse
import           Rho.TrackerComms.UDP.Request  as Req
import           Rho.TrackerComms.UDP.Response as Resp
import           Rho.TrackerComms.UDP.Types

-- | Channel to pass raw bytes read from socket to response handler.
type DataChan           = Chan (B.ByteString, SockAddr)

-- | Channel to pass `UDPResponse` of a particular transaction_id to it's
-- handler blocked while trying to read the `MVar UDPResponse`.
type TransactionChan    = MVar (M.Map TransactionId (MVar UDPResponse))

-- We need some state to manage UDP communications with trackers.
data UDPCommHandler = UDPCommHandler
  { sock             :: Socket
  , transactionChans :: TransactionChan
  }

initUDPCommHandler :: IO UDPCommHandler
initUDPCommHandler = do
    skt <- socket AF_INET Datagram defaultProtocol
    bind skt (SockAddrInet aNY_PORT 0)

    tChans <- newMVar M.empty

    -- data chan is used to push UDP packages to response handler
    dataChan <- newChan
    _ <- async $ sockListener skt dataChan
    _ <- async $ responseHandler dataChan tChans

    return $ UDPCommHandler skt tChans

-- | Socket listener reads stream from the socket and passes it to channel.
--
-- As far as I can see there's no way to handle `recvFrom` timeout
-- exceptions. Instead of hacking my way out, I decided to go with this:
-- This worker shouldn't be bothered with any events -- it just read
-- from the socket and pass the data. It only fails and terminates when
-- socket closed/main thread terminated.
sockListener :: Socket -> DataChan -> IO ()
sockListener skt dataChan = do
    (contents, src) <- recvFrom skt msg_size
    putStrLn $ "Got " ++ show (B.length contents) ++ " bytes from: " ++ show src
    writeChan dataChan (contents, src)
    sockListener skt dataChan
  where
    -- | Can't see anyting relevant in specs, but while testing I realized
    -- that trackers are always returning 1200 bytes. Using slightly bigger
    -- number here.
    msg_size = 1500

responseHandler :: DataChan -> TransactionChan -> IO ()
responseHandler dataChan tChan = do
    (resp, src) <- readChan dataChan
    putStrLn $ "Got response from " ++ show src
    case parseUDPResponse resp of
      Right msg ->
        modifyMVar_ tChan $ \chans -> do
          let tid' = Resp.tid msg
          case M.lookup tid' chans of
            Nothing -> do
              putStrLn $ "error: can't find transaction id handler for: " ++ show tid'
              return chans
            Just chan -> do
              putMVar chan msg
              return $ M.delete tid' chans
      Left err -> putStrLn $ "Can't parse server response: " ++ err
    responseHandler dataChan tChan

peerRequestUDP
  :: UDPCommHandler -> SockAddr -> PeerId -> InfoHash -> SessStats -> IO (Either String PeerResponse)
peerRequestUDP ch trackerAddr peerId infoHash (d, l, u) = do
    cid <- connectRequest ch trackerAddr
    case cid of
      Left err -> return $ Left err
      Right cid' -> announceRequest cid'
  where
    announceRequest :: ConnectionId -> IO (Either String PeerResponse)
    announceRequest cid = do
      annReqTid <- randomIO
      let msg = AnnounceRequest cid annReqTid infoHash peerId d l u Started
      annResp <- req ch trackerAddr msg
      case annResp of
        AnnounceResponse _ ps -> return $ Right ps
        _ -> return $ Left $ "Wrong response: " ++ show annResp

scrapeRequestUDP
  :: UDPCommHandler -> SockAddr -> [InfoHash] -> IO (Either String [(Word32, Word32, Word32)])
scrapeRequestUDP ch trackerAddr infos = do
    cid <- connectRequest ch trackerAddr
    case cid of
      Left err -> return $ Left err
      Right cid' -> scrapeRequest cid'
  where
    scrapeRequest :: ConnectionId -> IO (Either String [(Word32, Word32, Word32)])
    scrapeRequest cid = do
      scrapeReqTid <- randomIO
      scrapeResp <- req ch trackerAddr $ ScrapeRequest cid scrapeReqTid infos
      case scrapeResp of
        ScrapeResponse _ d -> return $ Right d
        _ -> return $ Left $ "Wrong response: " ++ show scrapeResp

connectRequest :: UDPCommHandler -> SockAddr -> IO (Either String ConnectionId)
connectRequest ch trackerAddr = do
    connReqTid <- randomIO
    connResp <- req ch trackerAddr $ ConnectRequest connReqTid
    case connResp of
      ConnectResponse _ cid -> return $ Right cid
      _ -> return $ Left $ "Wrong response: " ++ show connResp

req :: UDPCommHandler -> SockAddr -> UDPRequest -> IO UDPResponse
req UDPCommHandler{sock=skt, transactionChans=tChan} addr udpReq = do
    -- TODO: set timeouts
    let reqTid = Req.tid udpReq
    respVar <- newEmptyMVar
    modifyMVar_ tChan $ return . M.insert reqTid respVar
    sendAllTo skt (mkTrackerMsg udpReq) addr
    takeMVar respVar
