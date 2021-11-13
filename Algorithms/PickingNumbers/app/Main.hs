module Main where

import Data.List ( group, sort )

-- The lengths of given lists (added together if the lists numbers are adjacent (within 1))
adjacentLengths :: (Ord a, Num a) => [[a]] -> [Int]
adjacentLengths []     = [0]
adjacentLengths [a]    = [length a]
adjacentLengths (a:b:as)
  | abs (head a - head b) <= 1 = length a + length b : adjacentLengths (b:as)
  | otherwise                  = length a : adjacentLengths (b:as)

-- The maximum length subsequence of a given list of numbers where each number in the subsequence is within 1 of each other number
pickingNumbers :: (Ord a, Num a) => [a] -> Int
pickingNumbers numbers = maximum $ adjacentLengths $ group $ sort numbers

main :: IO ()
main = do
  _ <- readLn :: IO Int -- Read the number of numbers to be entered but don't bind it because we don't need it
  numbers <- map read . words <$> getLine :: IO [Int] -- Read list of numbers and bind it
  print $ pickingNumbers numbers -- Print the maximum length subsequence of the given list of numbers where each number in the subsequence is within 1 of each other number