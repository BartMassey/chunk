{-# LANGUAGE Rank2Types #-}

-- Copyright © 2015 Bart Massey
-- [This work is licensed under the "3-clause BSD License".]
-- Please see the file LICENSE in the source
-- distribution of this software for license terms.

import Criterion.Main
import System.Environment

import Chunk

-- Need constrained type for the type system to
-- figure out what is going on.
pow10 :: Int -> Int
pow10 i = 10^i

main :: IO ()
main = do
    (maxDigitsGSString : maxDigitsLSString : restArgs) <- getArgs
    let maxDigitsGS = read maxDigitsGSString :: Int
    let maxDigitsLS = read maxDigitsLSString :: Int
    withArgs restArgs $ defaultMain [
        genBench "groupsize"
         (\g l -> show l ++ "," ++ show g)
         ([1..5] ++ [10,20..50] ++ [pow10 i | i <- [2..maxDigitsGS]])
         [pow10 maxDigitsGS],
        genBench "listsize"
         (\g l -> show g ++ "," ++ show l)
         [1000]
         [pow10 i | i <- [3..maxDigitsLS]] ]

-- Given a label for the benchmark group and a labeling
-- function for the instances, generate a benchmark for each
-- group size and list size in the lists.
genBench :: String -> (Int -> Int -> String) -> [Int] -> [Int] -> Benchmark
genBench label labelf gSizes lSizes =
    bgroup label $ map bench1 [ (g, l) | g <- gSizes, l <- lSizes ]
    where
      bench1 (g, l) =
          bgroup (labelf g l) $ map bench1f [
            ("chunk", chunk),
            ("chunk_splitat", chunk_splitat) ]
          where
            bench1f (fn, f) =
                bench fn $ nf (flip f ([1..l] :: [Int])) g
