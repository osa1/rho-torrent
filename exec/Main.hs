module Main where

import           Rho.Magnet
import           Rho.Metainfo

import qualified Data.ByteString.Char8 as B
import           System.Environment    (getArgs)

main :: IO ()
main = do
    args <- getArgs
    -- contents <- B.readFile (head args)
    -- print $ parseMetainfo contents
    print $ parseMagnet (B.pack $ head args)
