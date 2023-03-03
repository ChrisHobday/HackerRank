module Main (main) where

import Control.Monad
  ( replicateM 
  , mapM_ )

-- The factorial of a given number
-- Example: factorial 5 = 120
factorial :: (Eq t, Num t) => t -> t
factorial 0 = 1
factorial number = number * factorial (number - 1)

-- A given number rounded to a given number of decimal places
-- Example: roundN 1.123456 4 = 1.1235
roundN :: (RealFrac a1, Integral b, Fractional a2) => a1 -> b -> a2
roundN number numberOfDecimalPlaces = (fromIntegral . round)  (number * (10^numberOfDecimalPlaces)) / (10^numberOfDecimalPlaces)

-- A given number a to the power of another given number b divided by the factorial of number b
-- Example: powerDividedByFactorial 2 3 = 1.3333333333333333
powerDividedByFactorial :: (Floating a, Eq a) => a -> a -> a
powerDividedByFactorial number number' = (number ** number') / factorial number'

-- A series expansion of e^x is given by: 1 + x + x^2/2! + x^3/3! ... + x^9/9!
-- Note: Rounded to 4 decimal places
evaluate :: (RealFrac a1, Floating a1, Fractional a2, Enum a1) => a1 -> a2
evaluate number = roundN (sum $ powerDividedByFactorial number <$> [0 .. 9]) 4

main :: IO ()
main = do
  numberOfNumberToBeEntered <- readLn :: IO Int -- Read and bind number of numbers to be entered

  numbers <- replicateM numberOfNumberToBeEntered $ do -- For each number to be entered...
    readLn :: IO Float -- Read number
  
  mapM_ (print . evaluate) numbers -- Print the entered list of numbers with the evaluate function applied to them
