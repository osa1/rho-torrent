-- | Requests to UDP trackers.
module Rho.TrackerComms.UDP.Request where

import qualified Data.ByteString            as B
import qualified Data.ByteString.Builder    as BB
import qualified Data.ByteString.Lazy       as LB
import           Data.Monoid
import           Data.Word
import           Network.Socket             (PortNumber (..))

import           Rho.InfoHash
import           Rho.PeerComms.PeerId
import           Rho.TrackerComms.UDP.Types

data AnnounceEvent = None | Completed | Started | Stopped deriving (Show, Eq)

announceEventBytes :: AnnounceEvent -> Word32
announceEventBytes None      = 0
announceEventBytes Completed = 1
announceEventBytes Started   = 2
announceEventBytes Stopped   = 3

data UDPRequest
  = ConnectRequest TransactionId
  | AnnounceRequest
      ConnectionId
      TransactionId
      InfoHash
      PeerId
      Word64 -- downloaded
      Word64 -- left
      Word64 -- uploaded
      AnnounceEvent
      PortNumber
  | ScrapeRequest ConnectionId TransactionId [InfoHash]
  deriving (Show, Eq)

tid :: UDPRequest -> TransactionId
tid (ConnectRequest tid')                  = tid'
tid (AnnounceRequest _ tid' _ _ _ _ _ _ _) = tid'
tid (ScrapeRequest _ tid' _)               = tid'

mkTrackerMsg :: UDPRequest -> B.ByteString
mkTrackerMsg (ConnectRequest tid') =
    LB.toStrict . BB.toLazyByteString $
      BB.word64BE 0x41727101980 <> BB.word32BE 0 <> BB.word32BE tid'
mkTrackerMsg (AnnounceRequest cid tid' infoHash pid downloaded left uploaded ev (PortNum port)) =
    LB.toStrict . BB.toLazyByteString . mconcat $
      [ BB.word64BE cid
      , BB.word32BE 1 -- action: announce
      , BB.word32BE tid'
      , BB.byteString $ unwrapInfoHash infoHash
      , BB.byteString $ unwrapPeerId pid
      , BB.word64BE downloaded
      , BB.word64BE left
      , BB.word64BE uploaded
      , BB.word32BE $ announceEventBytes ev
      , BB.word32BE 0 -- IP address FIXME
      , BB.word32BE 0 -- key
      , BB.word32BE (-1) -- numwant
      , BB.word16LE port
      ]
mkTrackerMsg (ScrapeRequest cid tid' infos) =
    LB.toStrict . BB.toLazyByteString . mconcat
      $ BB.word64BE cid
      : BB.word32BE 2
      : BB.word32BE tid'
      : map (BB.byteString . unwrapInfoHash) infos
