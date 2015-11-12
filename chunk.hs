module Main where

import Criterion.Main

-- | Given a positive chunk size and a list, group the list up
-- into chunks of the given size. Any shortfall is in the last chunk.
--
-- > chunk 3 [1..5] == [[1,2,3],[4,5]]
--
chunk :: Int -> [a] -> [[a]]
chunk n0 _ | n0 < 1 =
    error "Data.List.chunk: non-positive count"
chunk _ [] = []
chunk n0 es0 =
    go n0 es0
    where
      go _ [] = [[]]
      go 0 es = [] : go n0 es
      go n (e : es) =
          let c : cs = go (n - 1) es in
          (e : c) : cs

main :: IO ()
main = defaultMain [ bgroup "groupsize" genBenchGroup,
                     bgroup "listsize" genBenchList ]

genBenchGroup :: [Benchmark]
genBenchGroup =
    map bench1 subterms
    where
      lognList = 6
      nList = 10^lognList :: Int
      subterms = [1..9] ++ [10,20..90] ++ [10^i | i <- [2..lognList]]
      bench1 n =
          bench (show nList ++ "," ++ show n) $
          nf (chunk n) ([1..lognList] :: [Int])

genBenchList :: [Benchmark]
genBenchList =
    map bench1 subterms
    where
      subterms = [10^i | i <- [3..7] :: [Int]]
      bench1 n =
          bench ("1000," ++ show n) $
          nf (chunk 1000) ([1..n] :: [Int])

