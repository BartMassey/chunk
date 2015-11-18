-- Copyright © 2015 Bart Massey
-- [This work is licensed under the "3-clause BSD License".]
-- Please see the file LICENSE in the source
-- distribution of this software for license terms.

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
chunk n0 es0 =
    go n0 es0
    where
      go _ [] = [[]]
      go 0 es = [] : go n0 es
      go n (e : es) =
          let c : cs = go (n - 1) es in
          (e : c) : cs

-- | Alternate implementation of chunk using 'splitAt'.
chunk_splitat :: Int -> [a] -> [[a]]
chunk_splitat n _ | n < 1 =
    error "Data.List.chunk_splitat: non-positive count"
chunk_splitat _ [] = []
chunk_splitat n es =
    c : chunk_splitat n cs
    where
      (c, cs) = splitAt n es
