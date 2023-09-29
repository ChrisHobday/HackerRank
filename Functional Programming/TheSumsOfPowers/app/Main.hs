module Main (main) where

-- sumsOfPowers :: t1 -> t1 -> t2 -> a
sumsOfPowers baseNumber targetNumber powerNumber
  -- The result of the given base to the given power is equal to the target (a possible sum of powers is found)
  | result == targetNumber = 1
  -- The result of the given base to the given power is greater than the target (this is not a sum of powers and there are no more to be found)
  | result > targetNumber  = 0
  -- Otherwise the result is less than the target number (this is not a sum of powers, but there may be more)
  | otherwise              = sumsOfPowers nextBaseNumber (targetNumber - result) powerNumber + sumsOfPowers nextBaseNumber targetNumber powerNumber -- Look for possible sum of powers with the next base number and possible sums of powers with the next base number that may combine with the current result, and add them together
  where
    -- The result of the given base number to the given power number
    result         = baseNumber^powerNumber
    -- The next number to potentially use as a base number (the next natural number)
    nextBaseNumber = baseNumber + 1

main :: IO ()
main = do
  targetNumber <- readLn :: IO Int -- Read and bind the target number
  powerNumber <- readLn :: IO Int -- Read and bind the power number
  print $ sumsOfPowers 1 targetNumber powerNumber -- Print the sums of powers of the given target number and power number starting with the base number of 1
