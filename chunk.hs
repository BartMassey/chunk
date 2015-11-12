module Main where

import Criterion.Main
import System.Environment

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

-- Need constrained type for the type system to
-- figure out what is going on.
pow10 :: Int -> Int
pow10 i = 10^i

main :: IO ()
main = do
  mode : args <- getArgs
  case mode of
    "--criterion" -> withArgs args criterionMain
    _ -> error "chunk: bad mode"
         
criterionMain :: IO ()
criterionMain =
    defaultMain [
        genBench "groupsize"
         (\g l -> show l ++ "," ++ show g)
         ([1..9] ++ [10,20..90] ++ [pow10 i | i <- [2..6]])
         [pow10 6],
        genBench "listsize"
         (\g l -> show g ++ "," ++ show l)
         [1000]
         [pow10 i | i <- [3..7]] ]

-- Given a label for the benchmark group and a labeling
-- function for the instances, generate a benchmark for each
-- group size and list size in the lists.
genBench :: String -> (Int -> Int -> String) -> [Int] -> [Int] -> Benchmark
genBench label labelf gSizes lSizes =
    bgroup label $ map bench1 [ (g, l) | g <- gSizes, l <- lSizes ]
    where
      bench1 (g, l) =
          bench (labelf g l) $ nf (chunk g) ([1..l] :: [Int])
