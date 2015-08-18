module Rho.SessionState where

import           Control.Concurrent
import           Data.IORef
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import           Data.Word
import           Network.Socket              (PortNumber, SockAddr)

import           Rho.InfoHash
import           Rho.PeerComms.PeerConnState
import           Rho.PeerComms.PeerId
import           Rho.PieceMgr
import           Rho.PieceStats
import           Rho.Tracker

data Session = Session
  { sessPeerId            :: PeerId
    -- ^ our peer id
  , sessInfoHash          :: InfoHash
    -- ^ info hash of the torrent we're downloading/seeding
  , sessTrackers          :: MVar [Tracker]
    -- ^ trackers we use to request peers
  , sessPeers             :: MVar (M.Map SockAddr (IORef PeerConn))
    -- ^ connected peers
  , sessPieceMgr          :: MVar (Maybe PieceMgr)
    -- ^ piece manager for torrent data
  , sessPieceStats        :: MVar PieceStats
    -- TODO: Should this be an MVar?
    -- TODO: We should remove pieces that we completed from PieceStats.
    -- TODO: We should remove disconnected peers from PieceStats.
  , sessMIPieceMgr        :: MVar PieceMgr
    -- ^ piece manager for info dictionary
  , sessRequestedPieces   :: MVar (S.Set Word32)
    -- ^ set of pieces we've requested
  , sessPort              :: PortNumber
    -- ^ port number of the socket that we use for incoming handshakes
  , sessOnMIComplete      :: MVar (IO ())
    -- ^ callback to call when metainfo download completed
  , sessOnTorrentComplete :: MVar (IO ())
    -- ^ callback to call when torrent download completed
  , sessDownloaded        :: IORef Word64
  , sessUploaded          :: IORef Word64
  , sessCurrentOptUnchoke :: IORef (Maybe (IORef PeerConn))
    -- ^ Lucky peer chosen for optimistic unchoke. We rotate this in every 30
    -- seconds. When we unchoked the current peer can be seen in pcLastUnchoke
    -- field of PeerConn.
  }

initSession
  :: PeerId -> InfoHash -> PortNumber
  -> [Tracker] -> Maybe PieceMgr -> Maybe PieceMgr -> IO Session
initSession peerId infoHash port trackers pieces miPieces = do
    ts     <- newMVar trackers
    peers  <- newMVar M.empty
    pmgr   <- newMVar pieces
    miPMgr <- maybe newEmptyMVar newMVar miPieces
    pstats <- newMVar initPieceStats
    reqs   <- newMVar S.empty
    miCb   <- newMVar (return ())
    tCb    <- newMVar (return ())
    dr     <- newIORef 0
    ur     <- newIORef 0
    opt    <- newIORef Nothing
    return $ Session peerId infoHash ts peers pmgr pstats miPMgr reqs port miCb tCb dr ur opt

type SessStats = (Word64, Word64, Word64)

stats :: Session -> IO (Word64, Word64, Word64)
stats Session{sessDownloaded=d, sessUploaded=u, sessPieceMgr=pm} = do
    d'  <- readIORef d
    u'  <- readIORef u
    pm' <- readMVar pm
    let l = case pm' of
              Nothing -> 0
              Just PieceMgr{pmTotalSize=ts} -> ts - d'
    return (d', l, u')
