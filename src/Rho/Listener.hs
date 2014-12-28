{-# LANGUAGE MultiWayIf, RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-- | Motivation: Sometimes peers send messages in chunks. I think this is
-- because of several reasons:
-- * Limited bandwidth allocated to the connection by remote peer.
-- * Small MTU.
-- * ...
--
-- So sometimes a single `recv` call is not enough to successfully parse a
-- peer message.
--
-- To handle this while still keeping rest of the program pure and simple,
-- we handle buffering here. `initListener` spawns a listener thread and
-- buffers whatever it reads. `recvLen` blocks until either big-enough
-- message is read from the buffer or listener thread is stopped. (because
-- of end-of-stream or an error).
--
-- An alternative approach would be to use something like a `Pipe`(from
-- `pipes` library) but I think that would significantly increase
-- complexity of the code. Parsers would be pipes from `Word8` to
-- `PeerMsg`, peer listeners would be consumers etc. We'd still need
-- buffering, but that could be easily done with an extra `Pipe`.
--
-- Message listener would then just `await` for more bytes or fail with
-- a proper error message.
--
-- The main advantage of this approach over `Pipe`s approach is that with
-- this, rest of the code stayed same.
--
-- TODO: Errors are not handled properly. We probably need to propagate
-- errors through `recvLen` to be able to report/handle them.
--
module Rho.Listener where

import           Control.Concurrent.Async
import           Control.Concurrent.MVar
import           Control.Monad
import qualified Data.ByteString          as B
import qualified Data.Dequeue             as D
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
    -- ^ listener thread
  , stopped  :: MVar ()
    -- ^ to be able to block until listener is stopped
  }

-- | Spawn a listener thread.
initListener :: IO B.ByteString -> IO Listener
initListener recv = do
    deque <- newIORef (D.empty, 0)
    updated <- newEmptyMVar
    lock <- newMVar ()
    stopped <- newEmptyMVar
    listener <- async $ listen recv deque updated lock stopped
    return $ Listener deque updated lock listener stopped

-- | Try to receive message of given length. A smaller message is returned
-- when no new messages will arrive. (e.g. when listener is stopped for
-- some reason)
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
       | otherwise     -> do
           listenerStopped <- not `fmap` isEmptyMVar stopped
           if listenerStopped
             then do
               -- listener is stopped, then return whatever is in the buffer
               let m = mconcat $ D.takeFront (D.length deq) deq
               writeIORef deque (D.empty, 0)
               putMVar lock ()
               return m
             else do
               -- we need to block until buffer is updated
               _ <- tryTakeMVar updated
               -- let the listener update the buffer
               putMVar lock ()
               takeMVar updated
               -- buffer should be updated, recurse
               recvLen sl len

dequeue :: D.BankersDequeue B.ByteString -> Int -> (D.BankersDequeue B.ByteString, B.ByteString)
dequeue d 0   = (d, B.empty)
dequeue d len =
    let (Just bs, d') = D.popFront d in
    case compare (B.length bs) len of
      LT ->
       let (d'', bs') = dequeue d' (len - B.length bs) in
       (d'', bs <> bs')
      EQ -> (d', bs)
      GT ->
       let (h, t) = B.splitAt len bs in
       (D.pushFront d' t, h)

-- TODO: Test for exceptions and errors.
listen :: IO B.ByteString -> IORef Deque -> MVar () -> MVar () -> MVar () -> IO ()
listen recv deq updated lock stopped = catchIOError loop errHandler
  where
    loop = do
      stopped' <- not `fmap` isEmptyMVar stopped
      unless stopped' $ do
        bytes <- recv
        if | B.null bytes -> stop'
           | otherwise    -> do
               takeMVar lock
               modifyIORef' deq $ \(d, s) -> (D.pushBack d bytes, s + B.length bytes)
               _ <- tryPutMVar updated ()
               putMVar lock ()
               loop

    errHandler err = do
      putStrLn $ "Error happened while listening a socket: " ++ show err
      stop'

    stop' = tryPutMVar updated () >> putMVar stopped ()

stopListener :: Listener -> IO ()
stopListener (Listener _ u lock l s) =
    cancel l >> tryPutMVar u () >> tryPutMVar s () >> tryPutMVar lock () >> return ()
