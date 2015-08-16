-- | Exports some orphan instances.
module Rho.Instances where

import           Control.DeepSeq
import           Network.Socket  (PortNumber (..))

instance NFData PortNumber where
    rnf (PortNum n) = rnf n
