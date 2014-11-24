module Rho.ClientSpec where

import           System.Process

import           Test.Hspec
import           Test.Hspec.HUnit
import           Test.HUnit

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "client" $ do
      fromHUnitTest $ TestLabel "meeting using a tracker" meeting

meeting :: Test
meeting = TestCase $ do
    -- tracker <- spawnTracker
    -- putStrLn "Spawned a tracker."
    return ()

-- opentracker doesn't stop on SIGTERM...
spawnTracker :: IO ProcessHandle
spawnTracker = do
    (_, _, _, handle) <- createProcess (proc "opentracker" []){cwd=Just "tests/should_parse"}
    return handle
