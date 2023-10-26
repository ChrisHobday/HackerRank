module Main (main) where

import Data.List
  ( intercalate )

removeFirstSubsequence (x : xs) (y : ys) matches
  | x == y       = removeFirstSubsequence xs ys (matches <> [x])
  | null matches = y : removeFirstSubsequence (x : xs) ys []
  | otherwise    = matches <> removeFirstSubsequence (matches <> (x : xs)) (y : ys) []
removeFirstSubsequence [] ys _ = ys
removeFirstSubsequence _ [] matches = matches

kadaneBestSumAndSequence currentSum currentSequence bestSum bestSequence (number : numbers) = kadaneBestSumAndSequence newCurrentSum newCurrentSequence newBestSum newBestSequence numbers
  where
    (newCurrentSum, newCurrentSequence)
      = if number > (currentSum + number)
        then
          (number, [number])
        else
          (currentSum + number, currentSequence <> [number])
    (newBestSum, newBestSequence)
      = if newCurrentSum > bestSum
          then
            (newCurrentSum, newCurrentSequence)
          else
            (bestSum, bestSequence)
kadaneBestSumAndSequence _ _ bestSum bestSequence [] = (bestSum, bestSequence)

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
