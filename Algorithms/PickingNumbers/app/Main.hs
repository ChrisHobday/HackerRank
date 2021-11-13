module Main where

import Data.List ( subsequences )

-- A list of Bools representing whether the absolute difference between numbers in a given list is <= 1
isDifferenceOf1 :: (Ord a, Num a) => [a] -> [Bool]
isDifferenceOf1 []     = []
isDifferenceOf1 (a:as) = map ((<= 1) . abs . (-) a) as ++ isDifferenceOf1 as

-- The maximum length subsequence of a given list of numbers where each number in the subsequence is within 1 of each other number
pickingNumbers :: (Ord a, Num a) => [a] -> Int
pickingNumbers numbers = maximum $ map length $ filter ((==) True . and . isDifferenceOf1) (subsequences numbers)

main :: IO ()
main = do
  _ <- readLn :: IO Int -- Read the number of numbers to be entered but don't bind it because we don't need it
  numbers <- map read . words <$> getLine :: IO [Int] -- Read list of numbers and bind it
  print $ pickingNumbers numbers -- Print the maximum length subsequence of the given list of numbers where each number in the subsequence is within 1 of each other number
