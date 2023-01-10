module Main (main) where

import Control.Monad ( replicateM )
import Data.List.Plus

-- The factors of a given number
-- Example: factors 16 = [1,2,4,8,16]
factors :: Integral a => a -> [a]
factors n = factors' 1
  where -- Whether a number is a factor of another number
        -- Example: isAFactorOf 2 10 = True
        isAFactorOf :: Integral a => a -> a -> Bool
        isAFactorOf a n = n `mod` a == 0
        -- The rounded squareroot of n
        rndSqrtN :: Integral a => a
        rndSqrtN = round $ sqrt $ fromIntegral n
        -- Helper function for factors, allowing simpler type signature with original n not going out of scope while recursing
        -- The factors of n starting from a
        factors' a
          -- a is equal to the squareroot of n + 1 (There cannot be any more factors)
          | a == rndSqrtN + 1 = []
          -- There may be more factors
          | otherwise         = if a `isAFactorOf` n -- Check if a is a factor of n
                                  then if a == n `div` a -- Check if a is equal to n divided by a (This is the last possible factor)
                                    then [a] -- Add a to the factors list
                                    else [a] ++ factors' (a + 1) ++ [n `div` a] -- Add a to the front of factors list, the rest of the factors in between, and a's opposite factor to the end
                                  else factors' (a + 1) -- Check the next factor                           

-- Whether a number is prime or not
isPrime :: Integral a => a -> Bool
isPrime k = k > 1 && null [ x | x <- [2 .. round $ sqrt $ fromIntegral k], k `mod` x == 0]

downToZero2 n = downToZero2' n
  where downToZero2' a
          | a == 0    = 0
          -- a is prime
          | isPrime a = downToZero2' $ a - 1
          -- a has factors we can use to reduce it
          | otherwise = downToZero2' $ nFactors !! max 0 (length nFactors `div` 2)
                          where nFactors = factors n


main :: IO ()
main = do
  numberOfQueries <- readLn :: IO Int -- Read and bind number of queries to be entered
  queries <- replicateM numberOfQueries $ do -- Replicate the following action for each query
    readLn :: IO Int -- Read query
  print queries