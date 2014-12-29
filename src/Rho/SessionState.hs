module Rho.SessionState where

import           Control.Concurrent
import           Data.IORef
import qualified Data.Map                    as M
import           Data.Word
import           Network.Socket              (PortNumber, SockAddr)

import           Rho.InfoHash
import           Rho.PeerComms.PeerConnState
import           Rho.PeerComms.PeerId
import           Rho.PieceMgr

data Session = Session
  { sessPeerId            :: PeerId
    -- ^ our peer id
  , sessInfoHash          :: InfoHash
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
  , sessDownloaded        :: Word64
  , sessLeft              :: Word64
  , sessUploaded          :: Word64
  }

initSession :: PeerId -> InfoHash -> PortNumber -> Maybe PieceMgr -> Maybe PieceMgr -> IO Session
initSession peerId infoHash port pieces miPieces = do
    peers  <- newMVar M.empty
    pmgr   <- newMVar pieces
    miPMgr <- maybe newEmptyMVar newMVar miPieces
    miCb   <- newMVar (return ())
    tCb    <- newMVar (return ())
    return $ Session peerId infoHash peers pmgr miPMgr port miCb  tCb  0 0 0

type SessStats = (Word64, Word64, Word64)

stats :: Session -> (Word64, Word64, Word64)
stats Session{sessDownloaded=d, sessLeft=l, sessUploaded=u} = (d, l, u)
