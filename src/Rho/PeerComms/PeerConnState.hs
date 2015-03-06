module Rho.PeerComms.PeerConnState where

import qualified Data.ByteString         as B
import qualified Data.Map                as M
import           Data.Word
import           Network.Socket          (SockAddr, Socket)

import qualified Rho.Bitfield            as BF
import           Rho.Listener
import           Rho.PeerComms.Handshake
import           Rho.PeerComms.Message
import           Rho.PeerComms.PeerId

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
  , pcRequest        :: Maybe Word32
    -- ^ the piece data we're expecting this peer to send
    -- NOTE: this should probably be a set if we want to take the advantage
    -- of multiple requests. (`reqq` parameter etc.)
  , pcPieces         :: Maybe BF.Bitfield
    -- TODO: remove Maybe and initialize with empty bitfield
  , pcSock           :: Socket
    -- ^ socket connected to the peer
  , pcSockAddr       :: SockAddr
    -- ^ address of the peer
  , pcListener       :: Listener
    -- ^ Listener that listens the peer's socket
  , pcExtended       :: ExtendedMsgSupport
    -- ^ Supports BEP10
  , pcExtendedMsgTbl :: ExtendedPeerMsgTable
    -- ^ BEP10, extension table
  , pcMetadataSize   :: Maybe Word64
    -- ^ BEP9, metadata_size key of ut_metadata handshake
  , pcMaxPieceSize   :: Word32
    -- ^ piece length we use while requesting piece from the peer
  , pcReqq           :: Word32
    -- ^ `reqq` from extended handshake. from BEP 10:
    -- "An integer, the number of outstanding request messages this client
    -- supports without dropping any. The default in in libtorrent is 250."
  , pcClientName     :: Maybe B.ByteString
    -- ^ `v` from extended handshake. from BEP10:
    -- "Client name and version (as a utf-8 string). This is a much more
    -- reliable way of identifying the client than relying on the peer id
    -- encoding."
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

newPeerConn :: PeerId -> ExtendedMsgSupport -> Socket -> SockAddr -> Listener -> PeerConn
newPeerConn peerId extension sock sockAddr l =
  PeerConn True False True False peerId Nothing
           Nothing sock sockAddr l extension M.empty Nothing
           (2 ^ (14 :: Word32)) -- 16Kb for now, we may need to dynamically adjust the value
           250 -- default value of libtorrent
           Nothing
