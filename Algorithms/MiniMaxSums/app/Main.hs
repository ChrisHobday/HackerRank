module Main where

-- Import subsequences for generating all subsequence lists of a given sequence list
import Data.List ( subsequences )

-- Creates all subsequence combinations of a given length and sequence
combinations :: Int -> [a] -> [[a]]
combinations k ns = filter ((k==).length) $ subsequences ns

main :: IO ()
main = do
  intsTemp <- getLine -- Read list of integers
  let ints    = map (read :: String -> Int) (words intsTemp) -- Convert read String to integers
      sumCombinations = fmap sum (combinations 4 ints) -- Create all subsequence combinations of 4 on given list of integers
  putStrLn $ show (minimum sumCombinations) ++ " " ++ show (maximum sumCombinations) -- Print minimum and maximum sum combinations
