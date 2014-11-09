{-# LANGUAGE RankNTypes, TupleSections #-}

module Rho.Comms where

import           Control.Applicative
import           Control.Concurrent.Async
import           Control.Concurrent.Chan
import           Control.Concurrent.MVar
import           Data.Bits
import qualified Data.ByteString           as B
import qualified Data.ByteString.Builder   as BB
import qualified Data.ByteString.Lazy      as LB
import           Data.IORef
import qualified Data.Map                  as M
import           Data.Monoid
import           Data.Word
import           Network.Socket            hiding (recv, recvFrom, send, sendTo)
import           Network.Socket.ByteString
import           System.Random             (randomIO)

import           Rho.Magnet
import           Rho.Metainfo
import           Rho.Torrent
import           Rho.Tracker

type TransactionId = Word32
type ConnectionId  = Word64
type PeerId        = B.ByteString -- ^ 20-byte

type ConnectionCallback = ConnectionId -> IO ()

data PeerResponse = PeerResponse
  { prTransactionId :: TransactionId
  , prInterval      :: Word32
  , prLeechers      :: Word32
  , prSeeders       :: Word32
  , prPeers         :: [SockAddr]
  } deriving (Show)

sendConnectReq
  :: Socket -> SockAddr
  -> MVar (M.Map TransactionId ConnectionCallback)
  -> ConnectionCallback
  -> IO TransactionId
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
    return transactionId

sendPeersReq
  :: Socket -> SockAddr -> PeerId -> Torrent
  -> MVar (M.Map TransactionId ConnectionCallback)
  -> IO TransactionId
sendPeersReq sock targetAddr peerId torrent cbs = do
    transactionId <- randomIO
    _ <- sendConnectReq sock targetAddr cbs $ \connId -> do
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
      sent <- sendTo sock req targetAddr
      -- TODO: maybe check if sent == length of msg
      return ()
    -- return transaction id of announce request
    return transactionId

initCommHandler :: IO (Socket, MVar (M.Map TransactionId ConnectionCallback))
initCommHandler = do
    sock <- socket AF_INET Datagram defaultProtocol
    bind sock (SockAddrInet (fromIntegral (5432 :: Int)) 0)

    cbs <- newMVar M.empty
    dataChan <- spawnSockListener sock
    _ <- spawnResponseHandler dataChan cbs -- async (runResponseHandler dataChan)

    return (sock, cbs)

-- | Socket listener reads stream from the socket and passes it to channel.
spawnSockListener :: Socket -> IO (Chan (B.ByteString, SockAddr))
spawnSockListener sock = do
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
      (contents, src) <- recvFrom sock msg_size
      putStrLn $ "Got " ++ show (B.length contents) ++ " bytes from: " ++ show src
      writeChan dataChan (contents, src)
      sockListener dataChan

    -- | Can't see anyting relevant in specs, but while testing I realized
    -- that trackers are always returning 1200 bytes. Using slightly bigger
    -- number here.
    msg_size = 1500

spawnResponseHandler
  :: Chan (B.ByteString, SockAddr)
  -> MVar (M.Map TransactionId ConnectionCallback) -> IO ()
spawnResponseHandler dataChan cbs = do
    _ <- async responseHandler
    return ()
  where
    responseHandler :: IO ()
    responseHandler = do
        (resp, src) <- readChan dataChan
        putStrLn $ "Got response from " ++ show src
        case execParser resp readWord32 of
          Just (0, resp') -> handleConnectResp resp' cbs
          Just (1, resp') -> handleAnnounceResp resp'
          Just (2, resp') -> handleScrapeResp resp'
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

handleAnnounceResp :: B.ByteString -> IO ()
handleAnnounceResp bs = do
    putStrLn "handling announce response"
    case parseResp of
      Nothing -> putStrLn "Can't parse announce response."
      Just ret -> putStrLn $ "Announce response: " ++ show ret
  where
    parseResp :: Maybe PeerResponse
    parseResp = fmap fst . execParser bs $ do
      transactionId <- readWord32
      interval <- readWord32
      leechers <- readWord32
      seeders <- readWord32
      addrs <- readAddrs
      return $ PeerResponse transactionId interval leechers seeders addrs

    readAddrs :: Parser [SockAddr]
    readAddrs = do
      addr <- readAddr
      case addr of
        Nothing -> return []
        Just addr' -> (addr' :) <$> readAddrs

    readAddr :: Parser (Maybe SockAddr)
    readAddr = tryP $ do
      ip <- readWord32
      port <- readWord16
      return $ SockAddrInet (fromIntegral port) ip

handleScrapeResp :: B.ByteString -> IO ()
handleScrapeResp _ = putStrLn "handling scrape response"


-- * Utils

newtype Parser a = Parser { runParser :: B.ByteString -> Maybe (a, B.ByteString) }

instance Functor Parser where
    fmap f (Parser p) = Parser $ \bs -> do
      (v, bs') <- p bs
      return (f v, bs')

instance Applicative Parser where
    pure v = Parser $ \bs -> Just (v, bs)
    Parser fn <*> Parser v = Parser $ \bs -> do
      (fn', bs') <- fn bs
      (v', bs'') <- v bs'
      return (fn' v', bs'')

instance Monad Parser where
    return = pure
    Parser p >>= f = Parser $ \bs -> do
      (v, bs') <- p bs
      runParser (f v) bs'

execParser :: B.ByteString -> Parser a -> Maybe (a, B.ByteString)
execParser bs (Parser f) = f bs

tryP :: Parser a -> Parser (Maybe a)
tryP (Parser p) = Parser $ \bs ->
    case p bs of
      Nothing -> Just (Nothing, bs)
      Just (p', bs') -> Just (Just p', bs')

-- | Try to read Word32.
--
-- >>> :{
--   execParser (B.pack [1, 2, 3, 4])
--     ((,,,) <$> readWord <*> readWord <*> readWord <*> readWord)
-- :}
-- Just ((1,2,3,4),"")
--
readWord :: Parser Word8
readWord = Parser B.uncons

-- | Try to read Word16.
--
-- >>> execParser (B.pack [1, 0]) readWord16
-- Just (256,"")
--
readWord16 :: Parser Word16
readWord16 = do
    w1 <- readWord
    w2 <- readWord
    return $ fromIntegral w1 `shiftL` 8 + fromIntegral w2

-- | Try to read Word32 from big-endian byte string.
--
-- >>> execParser (B.pack [1, 2, 3, 4]) readWord32
-- Just (16909060,"")
--
readWord32 :: Parser Word32
readWord32 = do
    w1 <- readWord
    w2 <- readWord
    w3 <- readWord
    w4 <- readWord
    return $    fromIntegral w1 `shiftL` 24
              + fromIntegral w2 `shiftL` 16
              + fromIntegral w3 `shiftL` 8
              + fromIntegral w4

-- | Try to read Word64 from big-endian byte string.
--
-- >>> execParser (B.pack [1, 2, 3, 4, 5, 6, 7, 8, 45]) readWord64
-- Just (72623859790382856,"-")
--
readWord64 :: Parser Word64
readWord64 = do
    w1 <- readWord32
    w2 <- readWord32
    return $ fromIntegral w1 `shiftL` 32 + fromIntegral w2
