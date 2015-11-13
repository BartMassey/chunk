{-# LANGUAGE ScopedTypeVariables #-}

import Control.Exception
import System.Exit
import Test.HUnit
import Chunk

-- Adapted from http://stackoverflow.com/a/33266991/364875
assertExpectException :: (Exception e, Eq e) => String -> e ->
                         IO a -> Assertion
assertExpectException preface expected action = do
  r <- catches
    (action >> (return . Just) "no exception thrown")
    [ Handler (\e -> return (checkForExpectedException e)),
      -- AsyncException should be passed on so that ^C etc works.
      -- (Unless checking for AsyncException, in which case
      -- previous handler applies.)
      Handler (\(e::AsyncException) -> throw e),
      -- Wrong exception type.
      Handler (\(e::SomeException) ->
               return $ Just ("unexpected exception thrown: " ++ show e)) ]
  case r of
    Nothing  -> return ()
    Just msg -> assertFailure $ preface ++ ": " ++ msg
  where
    checkForExpectedException e
        | e == expected = Nothing
        | otherwise =
            -- Right exception type, wrong value.
            Just $ "wrong exception detail, expected " ++
                   show expected ++ ", got: " ++ show e

-- Naive implementation of chunk as test oracle.
naiveChunk :: Int -> [a] -> [[a]]
naiveChunk n l
    | n >= length l = [l]
    | otherwise =
        take n l : naiveChunk n (drop n l)

-- Exhaustively compare naive with actual chunk on small inputs.
makeCorrespondings :: Int -> Int -> Test
makeCorrespondings gMax nMax =
    TestList [makeCorresponding g n | g <- [1..gMax], n <- [1..nMax]]
    where
      makeCorresponding g n =
          "corresponding " ++ show g ++ "/" ++ show n ~:
          naiveChunk g [1..n] ~=? chunk g [1..n]

-- Law: concat (chunk g l) == l
-- Exhaustively test for small inputs.
makeFlattenings :: Int -> Test
makeFlattenings gnMax =
    TestList [makeFlattening g n | g <- [1..gnMax], n <- [1..gnMax]]
    where
      makeFlattening g n =
          "flattening " ++ show g ++ "/" ++ show n ~:
          [1..n] ~=? concat (chunk g [1..n])

nill :: [[()]]
nill = []

badList :: [Int]
badList = [1..5] ++ [undefined] ++ [7..10]

tests :: Test
tests = TestList [
         "specials" ~: TestList [
           "0 on empty list" ~:
             TestCase (assertExpectException "Data.List.chunk"
                      (ErrorCall "Data.List.chunk: non-positive count")
                      (evaluate $ chunk 0 [])),
           "0 on singleton list" ~:
             TestCase (assertExpectException "Data.List.chunk"
                      (ErrorCall "Data.List.chunk: non-positive count")
                      (evaluate $ chunk 0 [()])),
           "empty list" ~: [] ~=? chunk 1 nill ],
         "corresponding" ~: makeCorrespondings 5 9,
         "flattenings" ~: makeFlattenings 9,
         "strictness" ~: TestList [
           "strict ahead" ~: 3 ~=? chunk 3 badList !! 0 !! 2,
           "strict inside" ~: 3 ~=? chunk 7 badList !! 0 !! 2,
           "strict behind" ~: 9 ~=? chunk 7 badList !! 1 !! 1 ] ]

main :: IO ()
main = do
  Counts { errors = errs, failures = fails } <- runTestTT tests
  case (errs, fails) of
    (0, 0) -> return ()
    _ -> exitFailure
