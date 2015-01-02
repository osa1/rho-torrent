module Rho.SessionState where

import           Control.Applicative
import           Control.Concurrent
import           Data.IORef
import qualified Data.Map                    as M
import           Data.Word
import           Network.Socket              (PortNumber, SockAddr)

import           Rho.InfoHash
import           Rho.PeerComms.PeerConnState
import           Rho.PeerComms.PeerId
import           Rho.PieceMgr
import           Rho.Tracker

data Session = Session
  { sessPeerId            :: PeerId
    -- ^ our peer id
  , sessInfoHash          :: InfoHash
  , sessTrackers          :: MVar [Tracker]
  , sessPeers             :: MVar (M.Map SockAddr (IORef PeerConn))
    -- ^ connected peers
  , sessPieceMgr          :: MVar (Maybe PieceMgr)
    -- ^ piece manager for torrent data
  , sessMIPieceMgr        :: MVar PieceMgr
    -- ^ piece manager for info dictionary
  , sessPort              :: PortNumber
    -- ^ port number of the socket that we use for incoming handshakes
  , sessOnMIComplete      :: MVar (IO ())
    -- ^ callback to call when metainfo download completed
  , sessOnTorrentComplete :: MVar (IO ())
    -- ^ callback to call when torrent download completed
  , sessDownloaded        :: IORef Word64
  , sessLeft              :: IORef Word64
  , sessUploaded          :: IORef Word64
  }

initSession
  :: PeerId -> InfoHash -> PortNumber
  -> [Tracker] -> Maybe PieceMgr -> Maybe PieceMgr -> IO Session
initSession peerId infoHash port trackers pieces miPieces = do
    ts     <- newMVar trackers
    peers  <- newMVar M.empty
    pmgr   <- newMVar pieces
    miPMgr <- maybe newEmptyMVar newMVar miPieces
    miCb   <- newMVar (return ())
    tCb    <- newMVar (return ())
    dr     <- newIORef 0
    lr     <- newIORef 0
    ur     <- newIORef 0
    return $ Session peerId infoHash ts peers pmgr miPMgr port miCb tCb dr lr ur

type SessStats = (Word64, Word64, Word64)

stats :: Session -> IO (Word64, Word64, Word64)
stats Session{sessDownloaded=d, sessLeft=l, sessUploaded=u} =
    (,,) <$> readIORef d <*> readIORef l <*> readIORef u
