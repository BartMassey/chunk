import Criterion.Main

import Chunk

-- Need constrained type for the type system to
-- figure out what is going on.
pow10 :: Int -> Int
pow10 i = 10^i

main :: IO ()
main =
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