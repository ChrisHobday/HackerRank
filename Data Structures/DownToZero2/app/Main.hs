module Main (main) where

import qualified Data.Vector as V
import Control.Monad ( replicateM
                     , forM )

-- Note: The trick to this problem, once you wrap your head around what it's asking you to do in the problem statement, 
-- is to realise it is heavily related to factors and prime numbers.

-- The factors of a given number
-- Example: factors 16 = [1,2,4,8,16]
factors :: Integral a => a -> V.Vector a
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
        -- factors' :: Integral a => a -> V.Vector a
        factors' a
          -- a is equal to the squareroot of n + 1 (There cannot be any more factors)
          | a == rndSqrtN + 1 = V.empty
          -- There may be more factors
          | otherwise         = if a `isAFactorOf` n -- Check if a is a factor of n
                                  then if a == n `div` a -- Check if a is equal to n divided by a (This is the last possible factor)
                                    then V.singleton a -- Add a to the factors list
                                    else V.singleton a <> factors' (a + 1) <> V.singleton (n `div` a) -- Add a to the front of factors list, the rest of the factors in between, and a's opposite factor to the end
                                  else factors' (a + 1) -- Check the next factor                           

-- Whether a number is prime or not
-- Example: isPrime 4 = False
isPrime :: Integral a => a -> Bool
isPrime a = a > 1 && null [ x | x <- [2 .. round $ sqrt $ fromIntegral a], a `mod` x == 0]

precomputed n = V.last $ precomputed' V.empty
  where -- precomputed' :: V.Vector Int -> V.Vector Int
        precomputed' as
          | n < lengthAs  = as
          | V.null as     = precomputed' $ as <> V.singleton 0
          | lengthAs == 1 = precomputed' $ as <> V.singleton 1
          | lengthAs == 2 = precomputed' $ as <> V.singleton 2
          | lengthAs == 3 = precomputed' $ as <> V.singleton 3
          | otherwise     = if isPrime currentNumber
                              then precomputed' $ as <> V.singleton minusStep
                              else precomputed' $ as <> V.singleton minSteps
          where -- The length of as
                lengthAs = V.length as
                -- The current number we are calculating the number of steps for
                currentNumber = lengthAs
                -- The factors of the current number
                currentNumberFactors = factors currentNumber
                -- The smallest max number of all factor pairs of the current number
                smallestMaxFactor = currentNumberFactors V.! (length currentNumberFactors `div` 2)
                -- The number of steps to zero with minus 1 option chosen now
                minusStep = V.last as + 1
                -- The number of steps to zero with prime option chosen now
                primeStep = as V.! smallestMaxFactor + 1
                -- The minimum number of steps to zero for the current number
                minSteps  = min minusStep primeStep

main :: IO ()
main = do
  numberOfQueries <- readLn :: IO Int -- Read and bind number of queries to be entered
  queries <- replicateM numberOfQueries $ do -- Replicate the following action for each query
    readLn :: IO Int -- Read query
  
  -- let x = V.generate 10000 id
  -- forM x (\i ->
  --   forM)
    
  mapM_ (print . precomputed) queries -- Print the smallest number of steps to zero for each of the queries