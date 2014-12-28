module Rho.SessionState where

import           Control.Concurrent
import           Data.IORef
import qualified Data.Map                    as M
import           Network.Socket              (SockAddr)

import           Rho.InfoHash
import           Rho.PeerComms.Handshake
import           Rho.PeerComms.PeerConnState
import           Rho.PieceMgr

data Session = Session
  { sessPeerId            :: PeerId
    -- ^ our peer id
  , sessInfoHash          :: InfoHash
  , sessPeers             :: MVar (M.Map SockAddr (IORef PeerConn))
    -- ^ connected peers
  , sessPieceMgr          :: MVar (Maybe PieceMgr)
    -- ^ piece manager for torrent data
  , sessMIPieceMgr        :: MVar (Maybe PieceMgr)
    -- ^ piece manager for info dictionary
  , sessOnMIComplete      :: MVar (IO ())
    -- ^ callback to call when metainfo download completed
  , sessOnTorrentComplete :: MVar (IO ())
    -- ^ callback to call when torrent download completed
  }
