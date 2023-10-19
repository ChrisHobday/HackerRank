module Main (main) where

import Control.Monad
  ( mapM_ )

-- The sequence sums and leftover lists for the first integer of a given list onward
-- Ex: firstIntegerSequenceSumsAndLeftovers 0 [1,2,3] = [(1,[2,3]),(3,[3]),(6,[])]
firstIntegerSequenceSumsAndLeftovers :: Num t => t -> [t] -> [(t, [t])]
firstIntegerSequenceSumsAndLeftovers _ [] = []
firstIntegerSequenceSumsAndLeftovers previousSum (integer : integers) = (currentSum, integers) : firstIntegerSequenceSumsAndLeftovers currentSum integers
  where
    currentSum = previousSum + integer

-- TODO find a way to keep previous integers not used in calculating the sum prepended to the front of leftovers list
-- The sequence sums and leftover lists for all integers of a given list onward
allSequenceSumsAndLeftovers :: Num t => [t] -> [(t, [t])]
allSequenceSumsAndLeftovers [] = []
allSequenceSumsAndLeftovers allIntegers@(_ : restOfIntegers) = firstIntegerSequenceSumsAndLeftovers 0 allIntegers <> allSequenceSumsAndLeftovers restOfIntegers

sequenceSums integers = undefined

main :: IO ()
main = do
  (_ : _ : maximumNumberOfSums : _) <- (read <$>) . words <$> getLine :: IO [Int] -- Read and bind the maximum number of sums to output (ignore the number of integers to be entered as we do not need it)
  integers <- (read <$>) . words <$> getLine :: IO [Int] -- Read and bind the list of integers to calculate the list of sequence of sums of
  print integers
  -- print $ sequenceSums integers
  -- mapM_ putStrLn sequenceSums
