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
import           Control.Exception        (bracket_)
import           Control.Monad
import qualified Data.ByteString          as B
import qualified Data.ByteString.Lazy     as LB
import           Data.IORef
import           Data.Monoid
import qualified System.Log.Logger        as L

import           Rho.Utils

data Listener = Listener
  { buffer             :: IORef LB.ByteString
  , updated            :: MVar ()
    -- ^ to be able to block until buffer is updated
  , lock               :: MVar ()
    -- ^ we need to update `updated` and `buffer` without any intervention
  , listener           :: Async ()
    -- ^ listener thread
  , stopped            :: MVar ()
    -- ^ to be able to block until listener is stopped
  , recvHistoryTimeout :: Int
  , recvHistory        :: IORef [(Int, Int)]
    -- ^ (message size, receive time in ms) pairs. we only keep last
    -- `recvHistoryTimeout` seconds.
  , totalDownloaded    :: IORef Int
  }

-- | Spawn a listener thread. Timeout value is set to 5000 (5 seconds).
initListener :: IO B.ByteString -> IO Listener
initListener recv = initListener' recv 5000

-- | Spawn a listener thread. Download speed is calculated using the
-- timeout value.
initListener' :: IO B.ByteString -> Int -> IO Listener
initListener' recv dt = do
    buffer <- newIORef LB.empty
    updated <- newEmptyMVar
    lock <- newMVar ()
    stopped <- newEmptyMVar
    recvH <- newIORef []
    dld <- newIORef 0
    listener <- async $ listen recv buffer dt recvH dld updated lock stopped
    return $ Listener buffer updated lock listener stopped dt recvH dld

-- | Download speed in kbps, generated using bytes received in last
-- `recvHistoryTimeout` milliseconds.
downloadSpeed :: Listener -> IO Float
downloadSpeed Listener{recvHistoryTimeout=dt, recvHistory=recvH} = do
    ct <- currentTimeMillis
    recvs <- atomicModifyIORef' recvH $ \rs -> let rs' = filter (\(_, t) -> ct - t < dt) rs
                                                in (rs', rs')
    return $ fromIntegral (sum (map fst recvs)) / fromIntegral dt

-- | Try to receive message of given length. A smaller message is returned
-- when no new messages will arrive. (e.g. when listener is stopped for
-- some reason)
recvLen :: Listener -> Int -> IO B.ByteString
recvLen _               0   = return B.empty -- TODO: maybe signal an error?
recvLen sl@Listener{..} len = do
    -- NOTE: listener releases `lock` in case of an exception, so we need
    -- to use `tryPutMVar` instead of `putMVar` to handle that case.
    takeMVar lock
    buf <- readIORef buffer
    if | fromIntegral (LB.length buf) >= len -> do
           let (m, rest) = LB.splitAt (fromIntegral len) buf
           writeIORef buffer rest
           _ <- tryPutMVar lock ()
           return (LB.toStrict m)
       | otherwise     -> do
           listenerStopped <- not `fmap` isEmptyMVar stopped
           if listenerStopped
             then do
               -- listener is stopped, return whatever is in the buffer
               let m = LB.toStrict buf
               writeIORef buffer LB.empty
               _ <- tryPutMVar lock ()
               return m
             else do
               -- we need to block until buffer is updated
               _ <- tryTakeMVar updated
               -- let the listener update the buffer
               _ <- tryPutMVar lock ()
               takeMVar updated
               -- buffer should be updated, recurse
               recvLen sl len

listen
  :: IO B.ByteString -> IORef LB.ByteString -> Int -> IORef [(Int, Int)] -> IORef Int
  -> MVar () -> MVar () -> MVar () -> IO ()
listen recv buffer dt recvs dld updated lock stopped =
    bracket_ (return ()) releaseLocks loop
  where
    loop = do
      bytes <- recv
      atomicModifyIORef_ dld $ \d -> d + B.length bytes
      if | B.null bytes -> releaseLocks
         | otherwise    -> do
             takeMVar lock
             ct <- currentTimeMillis
             atomicModifyIORef_ recvs $ \rs ->
               (B.length bytes, ct) : filter (\(_, t) -> ct - t < dt) rs
             modifyIORef' buffer $ \d -> d <> LB.fromStrict bytes
             _ <- tryPutMVar updated ()
             putMVar lock ()
             loop

    releaseLocks =
      void (tryPutMVar updated () >> tryPutMVar stopped () >> tryPutMVar lock ())

stopListener :: Listener -> IO ()
stopListener Listener{updated=u, lock=lk, listener=l, stopped=s} =
    cancel l >> tryPutMVar u () >> tryPutMVar s () >> tryPutMVar lk () >> return ()

notice :: String -> IO ()
notice = L.noticeM "Rho.Listener"
