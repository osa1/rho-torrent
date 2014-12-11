module Rho.PeerComms.PeerConnState where

import qualified Data.ByteString         as B
import qualified Data.Map                as M
import           Data.Word
import           Network.Socket          (Socket)

import qualified Rho.Bitfield            as BF
import           Rho.InfoHash
import           Rho.PeerComms.Handshake
import           Rho.PeerComms.Message

-- | State of connection with a peer.
data PeerConn = PeerConn
  { pcPeerChoking    :: Bool
    -- ^ peer is choking us
  , pcPeerInterested :: Bool
    -- ^ peer interested in something that we have to offer
  , pcChoking        :: Bool
    -- ^ we're choking the peer
  , pcInterested     :: Bool
    -- ^ we're interested in something that peer has to offer
  , pcPeerId         :: PeerId
  , pcOffers         :: InfoHash
    -- ^ torrent that the peer offers
  , pcPieces         :: Maybe BF.Bitfield
    -- TODO: remove Maybe and initialize with empty bitfield
  , pcSock           :: Socket
    -- ^ socket connected to the peer
  , pcExtended       :: ExtendedMsgSupport
    -- ^ Supports BEP10
  , pcExtendedMsgTbl :: ExtendedPeerMsgTable
    -- ^ BEP10, extension table
  , pcMetadataSize   :: Maybe Word32
    -- ^ BEP9, metadata_size key of ut_metadata handshake
  }

instance Ord PeerConn where
  PeerConn{pcPeerId=pid1} `compare` PeerConn{pcPeerId=pid2} = pid1 `compare` pid2

instance Eq PeerConn where
  PeerConn{pcPeerId=pid1} == PeerConn{pcPeerId=pid2} = pid1 == pid2

-- I couldn't care less about `read . show == id` property. Implementing
-- this to be able to show PeerConns in tests. Haskell typeclasses are
-- annoying sometimes. I'd love to be able to pass a show function to
-- HUnit/QuickCheck etc.
instance Show PeerConn where
  show PeerConn{pcPeerId=PeerId pid} = "<Peer with id: " ++ show (B.dropWhile (== 0) pid) ++ ">"

newPeerConn :: PeerId -> InfoHash -> ExtendedMsgSupport -> Socket -> PeerConn
newPeerConn peerId infoHash extension sock =
    PeerConn True False True False peerId infoHash Nothing sock extension M.empty Nothing