import System.Exit
import Test.HUnit
import Chunk

tests :: Test
tests = TestList [
         "nil" ~: "chunk 1 []" ~: [] ~=? (chunk 1 [] :: [[()]]) ]

main :: IO ()
main = do
  Counts { errors = errs, failures = fails } <- runTestTT tests
  case (errs, fails) of
    (0, 0) -> return ()
    _ -> exitFailure
