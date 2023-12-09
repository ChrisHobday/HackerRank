module Main (main) where

import Control.Monad
  ( replicateM
  , mapM_ )

-- The factorial of a given number
-- Ex: factorial 3 = 3 * 2 * 1 = 6
factorial :: (Eq t, Num t) => t -> t
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- The binomial coefficient of two given numbers (choose k from n)
-- Ex: binomialCoefficient 8 3 = 56
binomialCoefficient :: Integral a => a -> a -> a
binomialCoefficient n k = factorial n `div` (factorial k * factorial (n - k))

-- The binomial coefficient of two given numbers (choose k from n) mod 100000007
-- Ex: count 1000 5 = 90672686
count :: Integral a => a -> a -> a
count n k = binomialCoefficient n k `mod` 100000007

main :: IO ()
main = do
  testCases <- readLn :: IO Int -- Read and bind the number of test cases to be entered

  counts <- replicateM testCases $ do -- For each test case do the following and bind the results (in the form of a list)
    (n : k : _) <- (read <$>) . words <$> getLine :: IO [Integer] -- Read and bind n and k (choose k from n)
    return (count n k) -- Return the count of n and k
  
  mapM_ print counts -- Print each count from the entered n and k's in each test case (on it's own line)
