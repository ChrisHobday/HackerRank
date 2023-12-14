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

-- addoneall x xs = zipWith ((++) . (map (x:))) ([]:xs) (xs ++ [[]])

-- subsequences' = concat . foldr addoneall [[[]]]

-- subsequences' :: [a] -> [[a]]
-- subsequences' []       = []
-- subsequences' [a]      = [[a]]
-- subsequences' (a : as) = [a] : subsequences' as <> [(a : as)]

-- subsets integers = sortBy (comparing length) (subsequences integers)

subsets integers = S.powerSet $ S.fromList integers

-- subsetSum integers = 

-- subsetSum integers s = findSubset $ subsets integers
--   where
--     findSubset (set : sets)
--       | sum set >= s = length set
--       | otherwise    = findSubset sets
--     findSubset [] = -1

main :: IO ()
main = do
  -- _ <- readLn :: IO Int -- Read but don't bind the upcoming list size
  -- integers <- (read <$>) . words <$> getLine :: IO [Int] -- Read and bind a list of integers
  -- testCases <- readLn :: IO Int -- Read and bind the number of test cases to be entered
  -- subsetSums <- replicateM testCases $ do -- For each testcase do the following and bind the list of results
  --   s <- readLn :: IO Int -- Read and bind the integer to find whether a non-empty subset from integers is greater than or equal to
  --   return $ subsetSum integers s -- Return the subset sum of the entered number and list of integers

  -- mapM_ print subsetSums -- Print the subset sums of the entered list of integers and each entered number

  return ()
