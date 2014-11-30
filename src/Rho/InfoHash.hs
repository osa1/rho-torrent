module Rho.InfoHash where

import qualified Data.ByteString as B

-- | 20-byte info_hash of torrent
-- TODO: Move this type to somewhere nice and remove this module.
newtype InfoHash = InfoHash { unwrapInfoHash :: B.ByteString } deriving (Show, Eq, Ord)
