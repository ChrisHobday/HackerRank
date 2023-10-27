module Main (main) where

import Data.List
  ( intercalate )
-- import qualified Data.Sequence as Seq

-- A given list (y : ys) with the first sequence of (x : xs) removed
-- Ex: removeFirstSubsequence [1,2] [1,2,3,4] [] = [3,4] 
removeFirstSubsequence (x : xs) (y : ys) matches
  -- Current elements are the same
  | x == y       = removeFirstSubsequence xs ys (matches <> [x])
  -- There are no previous matches
  | null matches = y : removeFirstSubsequence (x : xs) ys []
  -- Otherwise the current elements are different and there are no previous matches
  | otherwise    = matches <> removeFirstSubsequence (matches <> (x : xs)) (y : ys) []
-- There is no (x : xs)
removeFirstSubsequence [] ys _ = ys
-- There is no (y : ys)
removeFirstSubsequence _ [] matches = matches

-- The best subsequence sum and the subsequence itself of a given list of numbers
-- Ex: kadaneBestSumAndSequence 0 [] 0 [] [1,-2,5] = (5,[5])
kadaneBestSumAndSequence currentSum currentSequence bestSum bestSequence (number : numbers) = kadaneBestSumAndSequence newCurrentSum newCurrentSequence newBestSum newBestSequence numbers
  where
    -- The new current sum and sequence
    (newCurrentSum, newCurrentSequence)
      = if number > (currentSum + number)
        then
          (number, [number])
        else
          (currentSum + number, currentSequence <> [number])
    -- The new best sum and sequence
    (newBestSum, newBestSequence)
      = if newCurrentSum > bestSum
          then
            (newCurrentSum, newCurrentSequence)
          else
            (bestSum, bestSequence)
-- There are no given numbers
kadaneBestSumAndSequence _ _ bestSum bestSequence [] = (bestSum, bestSequence)

-- The best subsequence sum and leftover elements of a list of numbers
kadaneMaxSumAndLeftovers numbers = (bestSum, leftovers)
  where
    (bestSum, bestSequence) = kadaneBestSumAndSequence 0 [] 0 [] numbers
    leftovers               = removeFirstSubsequence bestSequence numbers []

-- A list of maximum sum sequences of a given list of numbers (up to the size of a given maximumNumberOfSums)
-- Ex: sequenceSums [1,2,3] 2 = [6]
sequenceSums :: (Ord a, Num t, Num a, Eq t) => [a] -> t -> [a]
sequenceSums numbers maximumNumberOfSums
  -- The maximumNumber of sums has been reached or there are no numbers in given list or the maximum sum sequence is negative
  | maximumNumberOfSums == 0 || null numbers || maximumSumSequence == 0 = []
  | otherwise                                                           = maximumSumSequence : sequenceSums leftovers (maximumNumberOfSums - 1)
  where
    -- The maximum sum sequence along with the leftover integer list
    (maximumSumSequence, leftovers) = kadaneMaxSumAndLeftovers numbers

main :: IO ()
main = do
  (_ : maximumNumberOfSums : _) <- (read <$>) . words <$> getLine :: IO [Int] -- Read and bind the maximum number of sums to output (ignore the number of numbers to be entered as we do not need it)
  numbers <- (read <$>) . words <$> getLine :: IO [Int] -- Read and bind the list of numbers to calculate the list of sequence of sums of
  putStrLn $ intercalate "\n" $ show <$> sequenceSums numbers maximumNumberOfSums -- Print the sequence of sums of a given list of numbers (up to the size of a given maximumNumberOfSums)
