module Main (main) where

import Data.List
  ( intercalate )

-- The sequence sums and leftover lists for the first integer of a given list onward (given unusedIntegers are prepended to the list of leftovers)
-- Ex: firstIntegerSequenceSumsAndLeftovers [] 0 [1,2,3] = [(1,[2,3]),(3,[3]),(6,[])]
firstIntegerSequenceSumsAndLeftovers :: Num t => [t] -> t -> [t] -> [(t, [t])]
-- There are no integers in given list
firstIntegerSequenceSumsAndLeftovers _ _ []                                          = []
-- There is at least 1 integer in given list
firstIntegerSequenceSumsAndLeftovers unusedIntegers previousSum (integer : integers) = (currentSum, unusedIntegers <> integers) : firstIntegerSequenceSumsAndLeftovers unusedIntegers currentSum integers
  where
    -- The current sequence sum
    currentSum = previousSum + integer

-- The sequence sums and leftover lists for all integers of a given list onward (given unusedIntegers are passed along and eventually prepended to the list of leftovers created in firstIntegerSequenceSumsAndLeftovers)
-- Ex: allSequenceSumsAndLeftovers [] [1,2,3] = [(1,[2,3]),(3,[3]),(6,[]),(2,[1,3]),(5,[1]),(3,[1,2])]
allSequenceSumsAndLeftovers :: Num t => [t] -> [t] -> [(t, [t])]
-- There are no integers in given list
allSequenceSumsAndLeftovers _ []                                                        = []
-- There is at least 1 integer in given list
allSequenceSumsAndLeftovers unusedIntegers allIntegers@(unusedInteger : restOfIntegers) = firstIntegerSequenceSumsAndLeftovers unusedIntegers 0 allIntegers <> allSequenceSumsAndLeftovers (unusedIntegers <> [unusedInteger]) restOfIntegers

-- A list of maximum sum sequences of a given list of integers (up to the size of a given maximumNumberOfSums)
-- Ex: sequenceSums [1,2,3] 2 = [6]
sequenceSums :: (Ord a, Num t, Num a, Eq t) => [a] -> t -> [a]
sequenceSums integers maximumNumberOfSums
  -- The maximumNumber of sums has been reached or there are no integers in given list or the maximum sum sequence is negative
  | maximumNumberOfSums == 0 || null integers || signum maximumSumSequence == (-1) = []
  | otherwise                                                                      = maximumSumSequence : sequenceSums leftovers (maximumNumberOfSums - 1)
  where
    -- The maximum sum sequence along with the leftover integer list
    (maximumSumSequence, leftovers) = maximum $ allSequenceSumsAndLeftovers [] integers

-- kadaneMaxSum currentSum bestSum (number : numbers) = kadaneMaxSum newCurrentSum newBestSum numbers
--   where
--     newCurrentSum = max number (currentSum + number)
--     newBestSum    = max bestSum newCurrentSum
-- kadaneMaxSum _ bestSum [] = bestSum

kadaneMaxSum currentSum currentSequence bestSum bestSequence leftovers (number : numbers) = kadaneMaxSum newCurrentSum newCurrentSequence newBestSum newBestSequence newLeftovers numbers
  where
    (newCurrentSum, newCurrentSequence, leftoverPrefix)
      = if number > (currentSum + number)
        then
          (number, [number], currentSequence)
        else
          (currentSum + number, currentSequence ++ [number], [])
    (newBestSum, newBestSequence, leftoverSuffix)
      = if bestSum > newCurrentSum
          then
            (bestSum, bestSequence, [])
          else
            (newCurrentSum, newCurrentSequence, [])
    newLeftovers  = leftovers ++ leftoverPrefix ++ leftoverSuffix
kadaneMaxSum currentSum currentSequence bestSum bestSequence leftovers [] = (bestSum, bestSequence, currentSequence, leftovers)




main :: IO ()
main = do
  (_ : maximumNumberOfSums : _) <- (read <$>) . words <$> getLine :: IO [Int] -- Read and bind the maximum number of sums to output (ignore the number of integers to be entered as we do not need it)
  integers <- (read <$>) . words <$> getLine :: IO [Int] -- Read and bind the list of integers to calculate the list of sequence of sums of
  putStrLn $ intercalate "\n" $ show <$> sequenceSums integers maximumNumberOfSums -- Print the sequence of sums of a given list of integers (up to the size of a given maximumNumberOfSums)
