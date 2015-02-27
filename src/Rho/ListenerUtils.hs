{-# LANGUAGE NondecreasingIndentation #-}

module Rho.ListenerUtils where

import qualified Data.ByteString as B
import           Data.Monoid     ((<>))

import           Rho.Listener
import           Rho.Utils

data RecvMsg = ConnClosed B.ByteString | Msg B.ByteString deriving (Show, Eq)

-- | Try to recv a message of length 68.
recvHandshake :: Listener -> IO RecvMsg
recvHandshake l = do
    msg <- recvLen l 68
    return $ (if B.length msg /= 68 then ConnClosed else Msg) msg

-- | Try to receive a 4-byte length-prefixed message.
recvMessage :: Listener -> IO RecvMsg
recvMessage l = do
    lengthPrefix <- recvLen l 4
    if B.length lengthPrefix /= 4
      then return $ ConnClosed lengthPrefix
      else do
    let [w1, w2, w3, w4] = B.unpack lengthPrefix
        len = mkWord32 w1 w2 w3 w4
    msg <- recvLen l (fromIntegral len)
    return $ Msg $ lengthPrefix <> msg
