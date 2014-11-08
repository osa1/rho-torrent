module Rho.Comms where

import           Control.Concurrent.Async
import           Control.Concurrent.Chan
import           Data.Bits
import qualified Data.ByteString           as B
import           Data.Word
import           Network.Socket            hiding (recv, recvFrom, send, sendTo)
import           Network.Socket.ByteString

initCommHandler :: IO ()
initCommHandler = do
    sock <- socket AF_INET Datagram defaultProtocol
    bind sock (SockAddrInet (fromIntegral (5432 :: Int)) 0)

    dataChan <- spawnSockListener sock
    spawnResponseHandler dataChan

    return ()

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
      (contents, source) <- recvFrom sock msg_size
      putStrLn $ "Got " ++ show (B.length contents) ++ " bytes from: " ++ show source
      writeChan dataChan (contents, source)
      sockListener dataChan

    -- | Can't see anyting relevant in specs, but while testing I realized
    -- that trackers are always returning 1200 bytes. Using slightly bigger
    -- number here.
    msg_size = 1500

spawnResponseHandler :: Chan (B.ByteString, SockAddr) -> IO ()
spawnResponseHandler dataChan = do
    (resp, source) <- readChan dataChan
    putStrLn $ "Got response from " ++ show source
    case readWord32 resp of
      Just (0, resp') -> handleConnectResp resp'
      Just (1, resp') -> handleAnnounceResp resp'
      Just (2, resp') -> handleScrapeResp resp'
      Just (n, _) -> putStrLn $ "Unknown response: " ++ show n
      Nothing -> putStrLn $ "Got ill-formed response"
    spawnResponseHandler dataChan

handleConnectResp :: B.ByteString -> IO ()
handleConnectResp _ = putStrLn "handling connect response"

handleAnnounceResp :: B.ByteString -> IO ()
handleAnnounceResp _ = putStrLn "handling announce response"

handleScrapeResp :: B.ByteString -> IO ()
handleScrapeResp _ = putStrLn "handling scrape response"

-- | Try to read Word32 from big-endian byte string.
--
-- >>> readWord32 (B.pack [1, 2, 3, 4])
-- Just (16909060,"")
--
readWord32 :: B.ByteString -> Maybe (Word32, B.ByteString)
readWord32 bs = do
    (w1, bs') <- B.uncons bs
    (w2, bs'') <- B.uncons bs'
    (w3, bs''') <- B.uncons bs''
    (w4, bs'''') <- B.uncons bs'''
    return (    fromIntegral w1 `shiftL` 24
              + fromIntegral w2 `shiftL` 16
              + fromIntegral w3 `shiftL` 8
              + fromIntegral w4
           , bs'''' )

-- | Try to read Word64 from big-endian byte string.
--
-- >>> readWord64 (B.pack [1, 2, 3, 4, 5, 6, 7, 8, 45])
-- Just (72623859790382856,"-")
--
readWord64 :: B.ByteString -> Maybe (Word64, B.ByteString)
readWord64 bs = do
    (w1, bs') <- readWord32 bs
    (w2, bs'') <- readWord32 bs'
    return (fromIntegral w1 `shiftL` 32 + fromIntegral w2, bs'')
