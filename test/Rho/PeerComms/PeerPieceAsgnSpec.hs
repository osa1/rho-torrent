{-# LANGUAGE NondecreasingIndentation #-}

module Rho.PeerComms.PeerPieceAsgnSpec where

import           Control.Applicative
import qualified Data.ByteString             as B
import qualified Data.ByteString.Char8       as BC
import           Data.IORef
import           Data.List
import qualified Data.Map                    as M
import           Data.Monoid
import qualified Data.Set                    as S
import           Data.Word

import           Test.Hspec
import           Test.Hspec.HUnit
import           Test.Hspec.QuickCheck
import           Test.HUnit
import           Test.QuickCheck

import           Rho.PeerComms.Handshake
import           Rho.PeerComms.PeerConnState
import           Rho.PeerComms.PeerPieceAsgn
import           Rho.PieceMgr

-- import           Debug.Trace

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "peer piece assignments" $ do
    fromHUnitTest $ TestLabel "should assign pieces with one providers first" $ TestCase $ do
      peerGen <- newPeerGen
      peer1 <- newPeer peerGen
      peer2 <- newPeer peerGen
      let missings      = [(0, 0, 1), (1, 0, 1)]
          peerMap       = M.fromList [ (peer1, S.singleton 0), (peer2, S.fromList [0, 1]) ]
          asgns         = M.fromList $ assignPieces missings peerMap
          expectedAsgns = M.fromList [ (peer1, (0, 0, 1)), (peer2, (1, 0, 1)) ]
      assertEqual "assignments are wrong" expectedAsgns asgns

    fromHUnitTest $ TestLabel "should be able to assign randomly" $ TestCase $ do
      peerGen <- newPeerGen
      peer1 <- newPeer peerGen
      peer2 <- newPeer peerGen
      let missings = [(0, 0, 1), (1, 0, 1)]
          peerMap  = M.fromList [ (peer1, S.fromList [0, 1]), (peer2, S.fromList [0, 1]) ]
          asgns    = assignPieces missings peerMap
      assertEqual "some pieces are not assigned" 2 (length asgns)
      assertEqual "some pieces are assigned multiple times" 2 (S.size $ S.fromList $ map snd asgns)
      assertEqual "some peers are assigned multiple times" 2 (S.size $ S.fromList $ map fst asgns)

    fromHUnitTest $ TestLabel "regression 1" $ TestCase $ do
      let peer     = mkPeerConn 0
          missings = [ (0, 0, 1), (1, 0, 1) ]
          peerMap  = M.singleton peer $ S.fromList [0, 1]
          asgns    = assignPieces missings peerMap
      assertEqual "peer is assigned more than once(or not assigned at all)" 1 (length asgns)

    modifyMaxSuccess (const 200) $
      prop ("every piece should be assigned to at most one peer, "
            ++ "every peer should be assigned at most once") $ do
        -- trace ("\ngenerating peers") $ do
        peers <- genPeers
        -- trace ("done: " ++ show peers ++ "\ngenerating missings") $ do
        missings <- genPieceData 100
        -- trace ("done: " ++ show missings ++ "\ngenerating peerPieces") $ do
        peerPieces <- genPeerPieces peers missings

        let example = showCounterExample missings peerPieces
        -- trace ("\nrunning example: " ++ example) $ do

        let asgns = assignPieces missings peerPieces
            prop1 = -- pieces should be assigned to at most one peer
              S.size (S.fromList (map snd asgns)) == length asgns
            prop2 = -- peers should be assigned at most once
              S.size (S.fromList (map fst asgns)) == length asgns

        return $ counterexample example (prop1 .&&. prop2)

showCounterExample :: [PieceData] -> M.Map PeerConn (S.Set Word32) -> String
showCounterExample pd s = concat
    [ "assignPieces "
    , show pd, " ("
    , "M.fromList [", intercalate "," (map showPeerPieces (M.toList s)), "]"
    , ")"
    ]
  where
    showPeerPieces :: (PeerConn, S.Set Word32) -> String
    showPeerPieces (pc, ws) =
      "(" ++ peerConnConstr pc ++ "," ++ wsConstr ws ++ ")"

    peerConnConstr :: PeerConn -> String
    peerConnConstr PeerConn{pcPeerId=PeerId pid} =
      let id = BC.unpack $ B.dropWhile (== 0) pid
      in "newPeerConn (mkPeerId " ++ id ++ ") undefined undefined undefined"

    wsConstr :: S.Set Word32 -> String
    wsConstr ws = "S.fromList " ++ show (S.toList ws)

genPeers :: Gen [PeerConn]
genPeers = do
    len <- arbitrary `suchThat` (< 100)
    return $ map (\i -> mkPeerConn i) [0..len-1]

genPieceData :: Int -> Gen [PieceData]
genPieceData max = do
    ps <- listOf $ choose (0, max)
    -- this one takes forever to finish:
    -- ps <- listOf $ arbitrary `suchThat` (<= fromIntegral max)
    return
      . map (\p -> (fromIntegral p, 0, 1)) -- make piece data
      . S.toList . S.fromList -- remove duplicates
      $ ps

genPeerPieces :: [PeerConn] -> [PieceData] -> Gen (M.Map PeerConn (S.Set Word32))
genPeerPieces []       _      = return M.empty
genPeerPieces (p : ps) pieces = do
    sublist <- S.fromList . map (\(p, _, _) -> p) <$> genSublist pieces
    rest <- genPeerPieces ps pieces
    return $ M.insert p sublist rest

genSublist :: [a] -> Gen [a]
genSublist [] = return []
genSublist (a : as) = do
    take <- arbitrary
    if take
      then do
        rest <- genSublist as
        return $ a : rest
      else genSublist as

type PeerGen = IORef Int

newPeerGen :: IO PeerGen
newPeerGen = newIORef 0

newPeer :: PeerGen -> IO PeerConn
newPeer pg = do
    next <- readIORef pg
    writeIORef pg (next + 1)
    return $ mkPeerConn next

mkPeerConn :: Int -> PeerConn
mkPeerConn i = newPeerConn (mkPeerId i) undefined undefined undefined

mkPeerId :: Int -> PeerId
mkPeerId i =
    let s = BC.pack $ show i
    in PeerId $ B.pack (replicate (20 - B.length s) 0) <> s
