module Main (main) where

import Data.List
  ( sortBy )

positiveSequences currentSum currentSums bestSum bestSums (number : numbers)
  -- Current number is negative and there is a best sum
  | number < 0 && bestSum > 0 = positiveSequences 0 (currentSum : currentSums) 0 (bestSum : bestSums) numbers

positiveSequences :: (Num a, Ord a) => a -> a -> [a] -> [a]
positiveSequences currentSum bestSum (number : numbers)
  -- | newCurrentSum > currentSum && bestSum > 0 = newBestSum : positiveSequences 0 0 numbers
  | newCurrentSum <= 0 && bestSum > 0         = newBestSum : positiveSequences 0 0 numbers
  | otherwise                                 = positiveSequences newCurrentSum newBestSum numbers
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

sequenceSums :: (Num a, Ord a) => [a] -> Int -> [a]
sequenceSums numbers maximumNumberOfSums = take maximumNumberOfSums (sortBy (flip compare) $ positiveSequences 0 0 numbers)

main :: IO ()
main = do
  (_ : maximumNumberOfSums : _) <- (read <$>) . words <$> getLine :: IO [Int] -- Read and bind the maximum number of sums to output (ignore the number of numbers to be entered as we do not need it)
  numbers <- (read <$>) . words <$> getLine :: IO [Int] -- Read and bind the list of numbers to calculate the list of sequence of sums of
  mapM_ putStrLn (show <$> sequenceSums numbers maximumNumberOfSums) -- Print the sequence sums of the given list of numbers and maximum number of sums
