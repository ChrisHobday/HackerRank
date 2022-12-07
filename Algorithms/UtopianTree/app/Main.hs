module Main (main) where

import Control.Monad ( replicateM )

utopianTree :: Int -> Int
utopianTree growthCycles
  -- No growth cycles (or an illegal negative amount)
  | growthCycles <= 0 = 1 -- Default/starting height of 1
  -- Even number of growth cycles
  | even growthCycles = 1 + utopianTree (growthCycles - 1) -- Add 1 to height 
  -- Otherwise (odd number of growth cycles)
  | otherwise         = 2 * utopianTree (growthCycles - 1) -- Double height

main :: IO ()
main = do
  numberTestCases <- readLn :: IO Int -- Read number of test cases to be entered
  testCases <- replicateM numberTestCases (readLn :: IO Int) -- Read list of test cases for utopian trees
  mapM_ (print . utopianTree) testCases -- Print utopian tree results for given utopian trees