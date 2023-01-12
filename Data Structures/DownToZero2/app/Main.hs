module Main (main) where

import qualified Data.Vector as V
import Control.Monad ( replicateM )

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

-- The smallest number of steps to get a number down to zero when you can only either 1) subtract the number by 1 or 2) use the max number of a factor pair
-- Example: downToZero2 5 = 4
-- downToZero2 :: Integral a => a -> Int
-- downToZero2 n = steps n
--   where -- The next step down to get a number down to zero
--         -- Example: stepDown 4 = 2
--         stepDown a
--           | a == 0              = 0
--           -- a is prime
--           | isPrime a || a == 1 = a - 1
--           -- a has factors we can use to reduce it
--           | otherwise           = if length minusStep <= length primeStep
--                                     then a - 1
--                                     else smallestMaxFactor
--                                     where -- The factors of a
--                                           aFactors          = factors a
--                                           -- The smallest max number of all factor pairs of the number
--                                           smallestMaxFactor = aFactors !! (length aFactors `div` 2)
--                                           -- The steps to zero with minus 1 option chosen now
--                                           minusStep         = steps (a - 1)
--                                           -- The steps to zero with prime option chosen now
--                                           primeStep         = steps smallestMaxFactor
--         -- The smallest list of steps to get a number down to zero
--         steps b
--           | stepDown b == 0 = [0]
--           -- There are more steps
--           | otherwise       = nextStep : steps nextStep
--                                 where nextStep = stepDown b

-- precomputed n = precomputed' []
--   where 
--     precomputed' as
--       | n < lengthAs  = as
--       | null as       = precomputed' $ 0 : as
--       | lengthAs == 1 = precomputed' $ 1 : as
--       | lengthAs == 2 = precomputed' $ 2 : as
--       | lengthAs == 3 = precomputed' $ 3 : as
--       | otherwise     = if isPrime currentNumber
--                           then precomputed' $ minusStep : as
--                           else precomputed' $ minSteps : as
--       where -- The current number we are calculating the number of steps for
--             currentNumber = lengthAs
--             -- The factors of the current number
--             currentNumberFactors = factors currentNumber
--             -- The smallest max number of all factor pairs of the current number
--             smallestMaxFactor = currentNumberFactors !! (length currentNumberFactors `div` 2)
--             -- The length of as
--             lengthAs = length as
--             -- The number of steps to zero with minus 1 option chosen now
--             minusStep = head as + 1
--             -- The number of steps to zero with prime option chosen now
--             primeStep = as !! (lengthAs - smallestMaxFactor - 1) + 1
--             -- The minimum number of steps to zero for the current number
--             minSteps  = min minusStep primeStep

precomputed n = V.head $ precomputed' V.empty
  where precomputed' :: V.Vector Int -> V.Vector Int
        precomputed' as
          | n < lengthAs  = as
          | V.null as     = precomputed' $ V.singleton 0 <> as
          | lengthAs == 1 = precomputed' $ V.singleton 1 <> as
          | lengthAs == 2 = precomputed' $ V.singleton 2 <> as
          | lengthAs == 3 = precomputed' $ V.singleton 3 <> as
          | otherwise     = if isPrime currentNumber
                              then precomputed' $ V.singleton minusStep <> as
                              else precomputed' $ V.singleton minSteps <> as
          where -- The length of as
                lengthAs = V.length as
                -- The current number we are calculating the number of steps for
                currentNumber = lengthAs
                -- The factors of the current number
                currentNumberFactors = factors currentNumber
                -- The smallest max number of all factor pairs of the current number
                smallestMaxFactor = currentNumberFactors V.! (length currentNumberFactors `div` 2)
                -- The number of steps to zero with minus 1 option chosen now
                minusStep = V.head as + 1
                -- The number of steps to zero with prime option chosen now
                primeStep = as V.! (lengthAs - smallestMaxFactor - 1) + 1
                -- The minimum number of steps to zero for the current number
                minSteps  = min minusStep primeStep                         

main :: IO ()
main = do
  numberOfQueries <- readLn :: IO Int -- Read and bind number of queries to be entered
  queries <- replicateM numberOfQueries $ do -- Replicate the following action for each query
    readLn :: IO Int -- Read query
  mapM_ (print . precomputed) queries -- Print the smallest number of steps to zero for each of the queries