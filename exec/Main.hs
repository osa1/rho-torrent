module Main where

import           Rho.Metainfo

import qualified Data.ByteString    as B
import           System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    contents <- B.readFile (head args)
    print $ parseMetainfo contents
