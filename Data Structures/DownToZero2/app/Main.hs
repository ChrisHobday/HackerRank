module Main (main) where

import Control.Monad (replicateM)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

-- Note: The trick to this problem, once you wrap your head around what it's asking you to do in the problem statement.
-- 1. Realise

-- The factors of a given number
-- Example: factors 16 = [1,2,4,8,16]
factors :: Integral a => a -> V.Vector a
factors n = factors' 1
  where
    -- Whether a number is a factor of another number
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
      | otherwise =
        if a `isAFactorOf` n -- Check if a is a factor of n
          then
            if a == n `div` a -- Check if a is equal to n divided by a (This is the last possible factor)
              then V.singleton a -- Add a to the factors list
              else V.singleton a <> factors' (a + 1) <> V.singleton (n `div` a) -- Add a to the front of factors list, the rest of the factors in between, and a's opposite factor to the end
          else factors' (a + 1) -- Check the next factor

-- Whether a number is prime or not
-- Example: isPrime 4 = False
isPrime :: Integral a => a -> Bool
isPrime a = a > 1 && null [x | x <- [2 .. round $ sqrt $ fromIntegral a], a `mod` x == 0]

precomputed n = precomputed' V.empty
  where
    -- precomputed' :: V.Vector Int -> V.Vector Int
    precomputed' as
      | n < lengthAs = as
      | V.null as = precomputed' $ as <> V.singleton 0
      | lengthAs == 1 = precomputed' $ as <> V.singleton 1
      | lengthAs == 2 = precomputed' $ as <> V.singleton 2
      | lengthAs == 3 = precomputed' $ as <> V.singleton 3
      | otherwise =
        if isPrime currentNumber
          then precomputed' $ as <> V.singleton minusStep
          else precomputed' $ as <> V.singleton minSteps
      where
        -- The length of as
        lengthAs = V.length as
        -- The current number we are calculating the number of steps for
        currentNumber = lengthAs
        -- The factors of the current number
        currentNumberFactors = factors currentNumber
        -- The smallest max number of all factor pairs of the current number
        smallestMaxFactor = currentNumberFactors V.! (V.length currentNumberFactors `div` 2)
        -- The number of steps to zero with minus 1 option chosen now
        minusStep = V.last as + 1
        -- The number of steps to zero with prime option chosen now
        primeStep = as V.! smallestMaxFactor + 1
        -- The minimum number of steps to zero for the current number
        minSteps = min minusStep primeStep

-- mutablePrecomputed n = do
--   mv <- MV.generate n 1000001 -- Create and bind mutable vector of n size, each index filled with it's own value (Example: MV.generate 10 id = [0,1,2,3,4,5,6,7,8,9])

--   MV.forM_ (MV.drop 3 mv) (\value -> do -- For each value of the mutable vector except the first 3
--     -- let currentNumberFactors = factors value
--     --     smallestMaxFactor = currentNumberFactors V.! (V.length currentNumberFactors `div` 2)
--     -- previousValue <- MV.read mv (value - 1)
--     -- primeStep <- MV.read mv (smallestMaxFactor + 1)
--     -- let minSteps  = min minusStep primeStep
--     --     minusStep = previousValue + 1

--     -- putStrLn $ "--------------------------------------"
--     -- putStrLn $ "currentNumber " ++ show value
--     -- putStrLn $ "currentNumberFactors " ++ show currentNumberFactors
--     -- putStrLn $ "smallestMaxFactor " ++ show smallestMaxFactor
--     -- putStrLn $ "minusStep " ++ show minusStep
--     -- putStrLn $ "primeStep " ++ show primeStep
--     -- putStrLn $ "minSteps " ++ show minSteps
--     -- putStrLn $ "--------------------------------------"

--     -- if isPrime value
--     --   then MV.write mv value minusStep
--     --   else MV.write mv value primeStep
--     )

--   V.freeze mv

mutablePrecomputed n = do
  mv <- MV.generate n id -- Create and bind mutable vector of n size, each index filled with it's own value (Example: MV.generate 10 id = [0,1,2,3,4,5,6,7,8,9])
  let sqrtN = round $ sqrt $ fromIntegral n
  
  MV.forM_ (MV.drop 3 mv) ( \value -> do -- For each value of the mutable vector except the first 3
    previousValue <- MV.read mv (value - 1)
    MV.write mv value (min value (previousValue + 1))

    let nDivValue = (n `div` value) - 1
    mv2 <- MV.generate nDivValue (1 +)
    putStrLn $ show value ++ "-----"
    
    MV.forM_ mv2 ( \x -> do
      currentValue <- MV.read mv (value * x)
      putStrLn $ "---" ++ show currentValue ++ " " ++ show (value + 1)
      MV.write mv (value * x) (min currentValue (value + 1))
      print x)
    
    )

  V.freeze mv

main :: IO ()
main = do
  -- numberOfQueries <- readLn :: IO Int -- Read and bind number of queries to be entered
  -- queries <- replicateM numberOfQueries $ do -- Replicate the following action for each query
  --   readLn :: IO Int -- Read query

  v <- mutablePrecomputed 10

  print v

-- mapM_ (print . precomputed) queries -- Print the smallest number of steps to zero for each of the queries
-- mapM_ precomputed queries -- Print the smallest number of steps to zero for each of the queries