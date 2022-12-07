module Main (main) where

import Control.Monad ( replicateM )

utopianTree :: Int -> Int
utopianTree n = 0

main :: IO ()
main = do
  numberTestCases <- readLn :: IO Int -- Read number of test cases to be entered
  testCases <- replicateM numberTestCases (readLn :: IO Int) -- Read list of test cases for growth cycles
  print $ map utopianTree testCases