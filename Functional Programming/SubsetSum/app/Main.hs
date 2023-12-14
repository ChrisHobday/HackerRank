module Main (main) where

import Data.Ord
  ( comparing )
import Data.List
  ( sortBy
  , subsequences )
import qualified Data.Set as S
  ( fromList, powerSet, map )
import Control.Monad
  ( replicateM
  , mapM_ )

-- The subsets of a given length and a given list
-- Ex: subsetsOf 2 [1,2,3] = [[1,2],[1,3],[2,3]]
subsetsOf :: (Eq t, Num t) => t -> [a] -> [[a]]
subsetsOf 0 _ = [[]]
subsetsOf _ [] = []
subsetsOf n (x : xs) = map (x :) (subsetsOf (n - 1) xs) ++ subsetsOf n xs

-- The subsets of a given list (in order) (excluding the empty set)
-- Ex: subsets [1,2,3] = [[1],[2],[3],[1,2],[1,3],[2,3],[1,2,3]]
subsets :: [a] -> [[a]]
subsets xs = subsets' 1 xs
  where
    -- A subfunction to encapsulate the n parameter
    subsets' n xs
      | n == length xs + 1 = []
      | otherwise          = subsetsOf n xs <> subsets' (n + 1) xs

-- The length of the smallest subset in a given list that is equal to or greater than a given number or -1 if there are none
subsetSum :: (Ord p, Num p) => [p] -> p -> Int
subsetSum integers s = findSubset $ subsets integers
  where
    findSubset (set : sets)
      | sum set >= s = length set
      | otherwise    = findSubset sets
    findSubset [] = -1

main :: IO ()
main = do
  _ <- readLn :: IO Int -- Read but don't bind the upcoming list size
  integers <- (read <$>) . words <$> getLine :: IO [Int] -- Read and bind a list of integers
  testCases <- readLn :: IO Int -- Read and bind the number of test cases to be entered
  subsetSums <- replicateM testCases $ do -- For each testcase do the following and bind the list of results
    s <- readLn :: IO Int -- Read and bind the integer to find whether a non-empty subset from integers is greater than or equal to
    return $ subsetSum integers s -- Return the subset sum of the entered number and list of integers

  mapM_ print subsetSums -- Print the subset sums of the entered list of integers and each entered number
