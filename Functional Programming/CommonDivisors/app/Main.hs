module Main (main) where

import Control.Monad
  ( replicateM
  , mapM_ )
import Data.List
  ( intersect )

-- The divisors of a given number
-- Ex: divisors 10 = [1,2,5,10]
divisors :: Integral a => a -> [a]
divisors n = [x | x <- [1..n], n `mod` x == 0]

-- The number of common divisors between two given numbers
-- Ex: commonDivisors 10 4 = 2
commonDivisors :: Integral a => a -> a -> Int
commonDivisors m l = length $ intersect (divisors m) (divisors l)

main :: IO ()
main = do
  testCases <- readLn :: IO Int -- Read and bind the number of tst cases to be entered

  allCommonDivisors <- replicateM testCases $ do -- For each testcase do the following and bind the list of results
    (m : l : _) <- (read <$>) . words <$> getLine :: IO [Int] -- Read and bind mario and luigi's points
    return $ commonDivisors m l -- Return the common divisors between mario and luigi's steps

  mapM_ print allCommonDivisors -- Print all the common divisors for each test case on their own lines