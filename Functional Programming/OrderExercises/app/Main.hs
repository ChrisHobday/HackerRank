module Main (main) where

import Data.List
  ( sortBy )
import Data.Maybe
  ( listToMaybe )

collapse (currentSum : currentSums) (bestSum : bestSums)
  -- This current best sum should be collapsed
  | currentSum - bestSum > 0 = ([currentSum], [])
  -- This current best sum should not be collapsed
  | otherwise                = (\(a, b) (as, bs) -> (a : as, b : bs)) (currentSum, bestSum) (collapse currentSums bestSums)
collapse [] _ = ([], [])
collapse (currentSum: currentSums) [] = ([currentSum], [])

-- lock (currentSum : currentSums) (potentialBestSum : potentialBestSums) bestSums
--   | currentSum < 0 = lock currentSums

positiveSequences previousNumberNegative currentSums bestSums (number : numbers) = positiveSequences numberNegative newCurrentSums newBestSums numbers                       
  where
    numberNegative = number < 0
    (newCurrentSums, newBestSums) =
      if numberNegative then
        if previousNumberNegative then
        -- Negative negative
          ((+ number) <$> currentSums, bestSums)
        else
        -- Positive negative
          -- ((+ number) <$> currentSums, bestSums <> [last currentSums])
          if not (null currentSums) then
              ((+ number) <$> currentSums, bestSums <> [last currentSums])
          else
            ((+ number) <$> currentSums, bestSums)
      else
        if previousNumberNegative then
        -- Negative positive (or first time being executed)
          if not (null currentSums) && head currentSums == 0 then
            collapse (((+ number) <$> tail currentSums) <> [number]) bestSums
          else
            collapse (((+ number) <$> currentSums) <> [number]) bestSums
        else
        -- Positive positive
          collapse ((+ number) <$> currentSums) bestSums

    -- newCurrentSums =
    --   -- Sequence has gone from negative to positive or is the first time being executed
    --   if not numberNegative && previousNumberNegative
    --   then
    --     number : ((+ number) <$> currentSums)
    --   else
    --     -- Check current sums to potentially lock some
    --     (+ number) <$> currentSums
    -- newBestSums =
    --   if numberNegative
    --   then
    --     if not previousNumberNegative
    --     then
    --       head currentSums : bestSums
    --     else
    --       bestSums
    --   else
    --     -- Check best sums to potentially collapse some
    --     collapse (tail newCurrentSums) bestSums

      -- -- Sequence has gone from positive to negative
      -- if number < 0 && not previousNumberNegative
      -- then
      --   head currentSums : bestSums
      -- else
      --   -- Check best sums to potentially collapse some
      --   bestSums

positiveSequences previousNumberNegative currentSums bestSums [] = newBestSums
  where
    newBestSums =
      -- There is a best sum in current sums to add to the list
      if not previousNumberNegative && not (null currentSums) then
          last currentSums : bestSums
      else
        bestSums

-- sequenceSums :: (Num a, Ord a) => [a] -> Int -> [a]
sequenceSums numbers maximumNumberOfSums = take maximumNumberOfSums (sortBy (flip compare) $ positiveSequences True [] [] numbers)

main :: IO ()
main = do
  (_ : maximumNumberOfSums : _) <- (read <$>) . words <$> getLine :: IO [Int] -- Read and bind the maximum number of sums to output (ignore the number of numbers to be entered as we do not need it)
  numbers <- (read <$>) . words <$> getLine :: IO [Int] -- Read and bind the list of numbers to calculate the list of sequence of sums of
  mapM_ putStrLn (show <$> sequenceSums numbers maximumNumberOfSums) -- Print the sequence sums of the given list of numbers and maximum number of sums



-- positiveSequences :: (Num a, Ord a) => a -> a -> [a] -> [a]
-- positiveSequences currentSum bestSum (number : numbers)
--   -- | newCurrentSum > currentSum && bestSum > 0 = newBestSum : positiveSequences 0 0 numbers
--   | newCurrentSum <= 0 && bestSum > 0         = newBestSum : positiveSequences 0 0 numbers
--   | otherwise                                 = positiveSequences newCurrentSum newBestSum numbers
--   where
--     newCurrentSum =
--       if number > (currentSum + number)
--       then
--         number
--       else
--         currentSum + number
--     newBestSum = max newCurrentSum bestSum
-- positiveSequences _ bestSum []
--   | bestSum > 0 = [bestSum]
--   | otherwise   = []

-- sequenceSums :: (Num a, Ord a) => [a] -> Int -> [a]
-- sequenceSums numbers maximumNumberOfSums = take maximumNumberOfSums (sortBy (flip compare) $ positiveSequences 0 0 numbers)

-- main :: IO ()
-- main = do
--   (_ : maximumNumberOfSums : _) <- (read <$>) . words <$> getLine :: IO [Int] -- Read and bind the maximum number of sums to output (ignore the number of numbers to be entered as we do not need it)
--   numbers <- (read <$>) . words <$> getLine :: IO [Int] -- Read and bind the list of numbers to calculate the list of sequence of sums of
--   mapM_ putStrLn (show <$> sequenceSums numbers maximumNumberOfSums) -- Print the sequence sums of the given list of numbers and maximum number of sums
