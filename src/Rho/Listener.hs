{-# LANGUAGE MultiWayIf, RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-- TODO: Add documentation. Add tests for:
-- * Errors and exceptions in listener thread.
-- * Blocking in recvLen.

module Rho.Listener where

import           Control.Concurrent.Async
import           Control.Concurrent.MVar
import qualified Data.ByteString           as B
import qualified Data.Dequeue              as D
import           Data.IORef
import           Data.Monoid
import           System.IO.Error

type Deque = (D.BankersDequeue B.ByteString, Int)

data Listener = Listener
  { deque    :: IORef Deque
    -- ^ buffer
  , updated  :: MVar ()
    -- ^ to be able to block until deque is updated
  , lock     :: MVar ()
    -- ^ we need to update `updated` and `deque` without any intervention
  , listener :: Async ()
    -- ^ listener threads
  , stopped  :: MVar ()
    -- ^ to be able to block until listener is stopped
  }

initListener :: IO B.ByteString -> IO Listener
initListener recv = do
    deque <- newIORef (D.empty, 0)
    updated <- newEmptyMVar
    lock <- newMVar ()
    stopped <- newEmptyMVar
    listener <- async $ listen recv deque updated lock stopped
    return $ Listener deque updated lock listener stopped

recvLen :: Listener -> Int -> IO B.ByteString
recvLen _               0   = return B.empty -- TODO: maybe signal an error?
recvLen sl@Listener{..} len = do
    takeMVar lock
    (deq, deqLen) <- readIORef deque
    if | deqLen >= len -> do
           let (d, m) = dequeue deq len
           writeIORef deque (d, deqLen - len)
           putMVar lock ()
           return m
       | otherwise       -> do
           -- we need to block until buffer is updated
           _ <- tryTakeMVar updated
           -- let the listener updated the buffer
           putMVar lock ()
           takeMVar updated
           -- buffer should be updated, recurse
           recvLen sl len

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

-- TODO: Test for exceptions and errors.
listen :: IO B.ByteString -> IORef Deque -> MVar () -> MVar () -> MVar () -> IO ()
listen recv deq updated lock stopped = flip catchIOError errHandler loop
  where
    loop = do
      bytes <- recv
      if | B.null bytes -> stop
         | otherwise    -> do
             takeMVar lock
             modifyIORef deq $ \(d, s) -> (D.pushBack d bytes, s + B.length bytes)
             _ <- tryPutMVar updated ()
             putMVar lock ()
             loop

    errHandler err = do
      putStrLn $ "Error happened while listening a socket: " ++ show err
      stop

    stop = tryPutMVar updated () >> putMVar stopped ()
