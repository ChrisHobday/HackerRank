module Main (main) where

import Control.Monad ( replicateM )

-- The greatest common divisor prime factorization between 2 given lists
-- Ex: gcdList [7,2] [2,2,7,1] = [7,1]
gcdList :: Ord a => [a] -> [a] -> [a]
-- Both lists have at least 2 elements (a primeNumber and power pair)
gcdList (primeNumber : power : primeFactorization) (primeNumber' : power' : primeFactorization')
  -- The current primeNumber of both lists is equal
  | primeNumber == primeNumber' = primeNumber : min power power' : gcdList primeFactorization primeFactorization'
  -- The primeNumber of the first list is greater than the primeNumber of the second list
  | primeNumber > primeNumber'  = gcdList (primeNumber : power : primeFactorization) primeFactorization'
  -- The primeNumber of the first list is less than the primeNumber of the second list
  | primeNumber < primeNumber'  = gcdList (primeNumber' : power' : primeFactorization') primeFactorization
-- The first list is empty
gcdList [] _ = []
-- The second list is empty
gcdList _ [] = []

main :: IO ()
main = do
  testCases <- readLn :: IO Int -- Read and bind the number of test cases to be entered
  (primeFactorization : restOfPrimeFactorizations) <- replicateM testCases $ do -- Replicate the following action the given number of test cases times
    (read <$>) . words <$> getLine :: IO [Int] -- Read, bind and return the prime factorization list
  putStrLn $ unwords $ show <$> foldl gcdList primeFactorization restOfPrimeFactorizations -- Print the greatest common divisor prime factorization between all the entered prime factorizations
