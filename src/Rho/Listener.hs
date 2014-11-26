{-# LANGUAGE MultiWayIf, RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Rho.Listener where

import           Control.Concurrent.Async
import           Control.Concurrent.Chan
import           Control.Concurrent.MVar
import           Control.Monad
import qualified Data.ByteString           as B
import qualified Data.Dequeue              as D
import           Data.IORef
import           Data.Maybe                (fromMaybe)
import           Data.Monoid
import           Network.Socket            hiding (KeepAlive, listen, recv,
                                            recvFrom, recvLen, send, sendTo)
import           Network.Socket.ByteString
import           System.IO.Error

type Deque = (D.BankersDequeue B.ByteString, Int)

data Listener = Listener
  { buffer    :: Chan B.ByteString
  , bufferLen :: MVar Int
  , deque     :: IORef Deque
  , listener  :: Async ()
  , stopped   :: MVar ()
  }

initListener :: IO B.ByteString -> IO Listener
initListener recv = do
    buffer <- newChan
    bufferLen <- newMVar 0
    stopped <- newEmptyMVar
    deque <- newIORef (D.empty, 0)
    listener <- async $ listen recv buffer bufferLen stopped
    return $ Listener buffer bufferLen deque listener stopped

recvLen :: Listener -> Int -> IO B.ByteString
recvLen sl@Listener{..} len = do
    bufferLen' <- takeMVar bufferLen
    clearBuffer bufferLen'
    ret <- atomicModifyIORef' deque $ \(deque, dequeLen) ->
      if | dequeLen >= len ->
             let (d, m) = dequeue deque len in
             ((d, dequeLen - len), Just m)
         | otherwise       -> ((deque, dequeLen), Nothing)
    case ret of
      Nothing  -> recvLen sl len
      Just msg -> return msg
  where
    clearBuffer :: Int -> IO ()
    clearBuffer 0 = return ()
    clearBuffer n = do
      msg <- readChan buffer
      atomicModifyIORef' deque $ \(deque, dequeLen) ->
        ((D.pushBack deque msg, dequeLen + B.length msg), ())
      clearBuffer (n - 1)

dequeue :: D.BankersDequeue B.ByteString -> Int -> (D.BankersDequeue B.ByteString, B.ByteString)
dequeue d len =
    let (Just bs, d') = D.popFront d in
    case compare (B.length bs) len of
      LT ->
       let (d'', bs') = dequeue d' (len - B.length bs) in
       (d'', bs <> bs')
      EQ -> (d', bs)
      GT ->
       let (h, t) = B.splitAt len bs in
       (D.pushFront d t, h)

-- TODO: exceptions and errors
listen :: IO B.ByteString -> Chan B.ByteString -> MVar Int -> MVar () -> IO ()
listen recv buf bufLen stopped = flip catchIOError errHandler loop
  where
    loop = do
      bytes <- recv
      if | B.null bytes -> stop
         | otherwise    -> do
             writeChan buf bytes
             bufLen' <- tryTakeMVar bufLen
             putMVar bufLen (fromMaybe 0 bufLen' + 1)
             loop

    errHandler err = do
      putStrLn $ "Error happened while listening a socket: " ++ show err
      stop

    stop = putMVar stopped ()
