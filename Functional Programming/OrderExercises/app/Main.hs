module Main (main) where

-- import qualified Data.Sequence as Seq
-- import qualified Data.Array as Arr
-- import qualified Data.Vector as Vec
import Data.Maybe
-- import Data.List.Split
import Data.Foldable
  ( toList )

positiveSequences currentSum bestSum (number : numbers)
  | number > (currentSum + number) = newBestSum : positiveSequences newCurrentSum 0 numbers
  | otherwise                      = positiveSequences newCurrentSum newBestSum numbers
  where
    newCurrentSum =
      if number > (currentSum + number)
      then
        number
      else
        currentSum + number
    newBestSum = max newCurrentSum bestSum
positiveSequences _ bestSum []
  | bestSum > 0 = [bestSum]
  | otherwise   = []


-- kadaneBestSumAndSequence currentSum currentSequence bestSum bestSequence potentialLeftovers (number : numbers) = kadaneBestSumAndSequence newCurrentSum newCurrentSequence newBestSum newBestSequence newPotentialLeftovers numbers
--   where
--     (newCurrentSum, newCurrentSequence)
--       = if number > (currentSum + number)
--         then
--           (number, [number])
--         else
--           (currentSum + number, number : currentSequence)
--     (newBestSum, newBestSequence)
--       = if newCurrentSum > bestSum
--           then
--             (newCurrentSum, newCurrentSequence)
--           else
--             (bestSum, bestSequence)
-- kadaneBestSumAndSequence _ _ bestSum bestSequence [] = (bestSum, reverse bestSequence)

-- sequenceSums numbers maximumNumberOfSums
--   -- The maximumNumber of sums has been reached or there are no numbers in given list or the maximum sum sequence is negative
--   | maximumNumberOfSums == 0 || Vec.null numbers || bestSum == 0 = []
--   | otherwise                                                    = bestSum : sequenceSums leftovers (maximumNumberOfSums - 1)
--   where
--     -- The maximum sum sequence along with the leftover integer list
--     (bestSum, leftovers) = kadaneBestSumAndLeftovers 0 (0, 0) 0 (0, 0) numbers 0

main :: IO ()
main = do
  -- (_ : maximumNumberOfSums : _) <- (read <$>) . words <$> getLine :: IO [Int] -- Read and bind the maximum number of sums to output (ignore the number of numbers to be entered as we do not need it)
  -- numbers <- (read <$>) . words <$> getLine :: IO [Int] -- Read and bind the list of numbers to calculate the list of sequence of sums of
  -- mapM_ putStrLn (show <$> sequenceSums (Vec.fromList numbers) maximumNumberOfSums) -- Print the sequence sums of the given list of numbers and maximum number of sums
  return ()
