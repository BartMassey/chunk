module Chunk where

-- | Given a positive chunk size and a list, group the list up
-- into chunks of the given size. Any shortfall is in the last chunk.
--
-- > chunk 3 [1..5] == [[1,2,3],[4,5]]
--
chunk :: Int -> [a] -> [[a]]
chunk n0 _ | n0 < 1 =
    error "Data.List.chunk: non-positive count"
chunk _ [] = []
chunk n0 es =
    go n0 es
    where
      go _ [e] = [[e]]
      go 1 (e : es) = [e] : go n0 es
      go n (e : es) =
          let c : cs = go (n - 1) es in
          (e : c) : cs
