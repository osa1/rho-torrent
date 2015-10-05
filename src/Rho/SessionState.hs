{-# LANGUAGE TupleSections #-}

module Rho.SessionState where

import           Control.Concurrent
import           Control.Monad
import           Data.IORef
import           Data.List                   (sortOn)
import qualified Data.Map                    as M
import           Data.Maybe                  (fromJust)
import qualified Data.Set                    as S
import           Data.Word
import           Network.Socket              (PortNumber, SockAddr)
import           Safe                        (headMay)

import           Rho.InfoHash
import           Rho.PeerComms.PeerConnState
import           Rho.PeerComms.PeerId
import           Rho.PieceMgr
import qualified Rho.PieceStats              as PS
import           Rho.Tracker
import           Rho.Utils

type PeerConnRef  = (PeerId, IORef PeerConn)
type PeerConnRef' = (PeerConn, IORef PeerConn)

readPeerConnRef :: PeerConnRef -> IO PeerConnRef'
readPeerConnRef (_, ref) = (, ref) <$> readIORef ref

peerConn :: PeerConnRef -> IO PeerConn
peerConn = readIORef . snd

data Session = Session
  { sessPeerId            :: PeerId
    -- ^ our peer id
  , sessInfoHash          :: InfoHash
    -- ^ info hash of the torrent we're downloading/seeding
  , sessTrackers          :: MVar [Tracker]
    -- ^ trackers we use to request peers
  , sessPeers             :: MVar (M.Map PeerId (IORef PeerConn))
    -- ^ connected peers
  , sessPieceMgr          :: MVar (Maybe PieceMgr)
    -- ^ piece manager for torrent data
  , sessPieceStats        :: MVar PS.PieceStats
    -- TODO: Should this be an MVar?
    -- TODO: We should remove pieces that we completed from PieceStats.
    -- TODO: We should remove disconnected peers from PieceStats.
  , sessMIPieceMgr        :: MVar PieceMgr
    -- ^ piece manager for info dictionary
  , sessRequestedPieces   :: MVar (S.Set PieceIdx)
    -- ^ set of pieces we've requested
  , sessPort              :: PortNumber
    -- ^ port number of the socket that we use for incoming handshakes
  , sessOnMIComplete      :: MVar (IO ())
    -- ^ callback to call when metainfo download completed
  , sessOnTorrentComplete :: MVar (IO ())
    -- ^ callback to call when torrent download completed
  , sessDownloaded        :: IORef Word64
  , sessUploaded          :: IORef Word64
  , sessCurrentOptUnchoke :: IORef (Maybe PeerConnRef)
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
    pstats <- newMVar PS.initPieceStats
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

--------------------------------------------------------------------------------

-- | Check whether we've an active connection with the given socket address.
checkActiveConnection :: Session -> SockAddr -> IO (Maybe PeerConn)
checkActiveConnection sess addr = do
    peers <- mapM readIORef =<< M.elems <$> readMVar (sessPeers sess)
    return $ headMay $ filter ((addr ==) . pcSockAddr) peers

readPeers :: Session -> IO [PeerConnRef']
readPeers = mapM readPeerConnRef . M.toList <=< readMVar . sessPeers

peerConnRefFromId :: Session -> PeerId -> IO (Maybe PeerConnRef)
peerConnRefFromId Session{sessPeers=peers} pId = ((pId,) <.> M.lookup pId) <$> readMVar peers

currentOptUnchoke :: Session -> IO (Maybe PeerConnRef)
currentOptUnchoke = readIORef . sessCurrentOptUnchoke

setCurrentOptUnchoke :: Session -> Maybe PeerConnRef -> IO ()
setCurrentOptUnchoke = writeIORef . sessCurrentOptUnchoke

addPiece :: Session -> PeerId -> PieceIdx -> IO ()
addPiece Session{sessPieceStats=statsMV} pid pIdx =
    modifyMVar_ statsMV $ return . PS.addPiece pid pIdx

addPieces :: Session -> PeerId -> [PieceIdx] -> IO ()
addPieces Session{sessPieceStats=statsMV} pid pIdx =
    modifyMVar_ statsMV $ return . PS.addPieces pid pIdx

piecesToReq :: Session -> IO [(PieceIdx, S.Set PeerId)]
piecesToReq sess = do
    stats <- readMVar (sessPieceStats sess)
    alreadyRequested <- readMVar (sessRequestedPieces sess)
    return $ filter (not . (flip S.member alreadyRequested) . fst) (PS.takeMins stats 10)

markPieceComplete :: Session -> PieceIdx -> IO ()
markPieceComplete Session{sessPieceStats=ps} pIdx =
    modifyMVar_ ps $ return . PS.removePiece pIdx

pieceReqForPeer :: Session -> PeerConn -> IO (Maybe PieceIdx)
pieceReqForPeer sess pc = do
    missings <- (S.fromList <.> missingPieces) =<< (fromJust <$> readMVar (sessPieceMgr sess))
    peerPcs  <- peerPieces pc
    reqs     <- readMVar (sessRequestedPieces sess)
    pStats   <- readMVar (sessPieceStats sess)

    let
      canRequest = S.toList ((missings `S.intersection` peerPcs) `S.difference` reqs)
      pris       = zip canRequest (map (flip PS.piecePriority pStats) canRequest)
      prisSorted = sortOn snd [ (pIdx, pri) | (pIdx, Just pri) <- pris ]

    return $ fst <$> headMay prisSorted
