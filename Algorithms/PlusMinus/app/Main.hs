module Main where

-- Import printf for decimal precision
import Text.Printf ( printf )

-- Counts the number of positive numbers in a list
countPositiveNums :: [Int] -> Int
countPositiveNums xs = length $ filter (\x -> signum x == 1) xs

-- Counts the number of negative numbers in a list
countNegativeNums :: [Int] -> Int
countNegativeNums xs = length $ filter (\x -> signum x == -1) xs

-- Counts the number of zeros in a list
countZeros :: [Int] -> Int
countZeros xs = length $ filter (==0) xs

main :: IO ()
main = do
  n <- readLn :: IO Int  -- Read number of integers to be entered
  intsTemp <- getLine -- Read list of integers
  let ints    = map (read :: String -> Int) (words intsTemp) -- Convert read String to integers
      numPos  = fromIntegral (countPositiveNums ints) / fromIntegral n
      numNeg  = fromIntegral (countNegativeNums ints) / fromIntegral n
      numZero = fromIntegral (countZeros ints) / fromIntegral n
  printf "%.6f\n" (numPos :: Double) -- Print ratio of positive numbers in given list
  printf "%.6f\n" (numNeg :: Double) -- Print ratio of negative numbers in given list
  printf "%.6f\n" (numZero :: Double) -- Print ratio of zeros in given list
