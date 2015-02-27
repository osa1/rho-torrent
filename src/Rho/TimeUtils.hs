-- | Utilities to work on 'TimeSpec's.
--
-- FIXME: Document functions and test.
--
module Rho.TimeUtils where

import           System.Clock

dt :: TimeSpec -> TimeSpec -> TimeSpec
dt (TimeSpec s1 ns1) ts2@(TimeSpec s2 ns2)
  | ns1 < ns2 = dt (TimeSpec (s1 - 1) (ns1 + 10^9)) ts2
  | otherwise = TimeSpec (s1 - s2) (ns1 - ns2)

addTs :: TimeSpec -> TimeSpec -> TimeSpec
addTs (TimeSpec s1 ns1) (TimeSpec s2 ns2) =
    let ns3 = ns1 + ns2
    in if ns3 >= 10^9
         then TimeSpec (s1 + s2 + 1) (ns3 - 10^9)
         else TimeSpec (s1 + s2)     ns3

tsToNs :: TimeSpec -> Integer
tsToNs (TimeSpec s ns) = fromIntegral s * 10^9 + fromIntegral ns

tsToSec :: TimeSpec -> Int
tsToSec (TimeSpec s _) = s

nsToTs :: Integer -> TimeSpec
nsToTs i = let (s, ns) = i `divMod` (10^9)
           in TimeSpec (fromIntegral s) (fromIntegral ns)
