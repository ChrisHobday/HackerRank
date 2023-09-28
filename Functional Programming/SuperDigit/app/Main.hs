module Main (main) where

import Data.List ( genericReplicate )

-- A given number's digit summed together recursively until the number is only a single digit
-- Ex: superDigit 1234 = 1
superDigit :: (Ord t, Num t, Read t, Show t) => t -> t
superDigit numberToSumDigitsOf
  -- The summed digits is greater than 9 (more than 1 digit)
  | summedDigits > 9 = superDigit summedDigits
  -- Otherwise (The summed digits is 9 or less (only 1 digit))
  | otherwise        = summedDigits
  where
    -- The given number's digits summed together
    summedDigits = sum (read <$> ((:"") <$> show numberToSumDigitsOf))

main :: IO ()
main = do
  (number : numberOfTimesToReplicate : _) <- map read . words <$> getLine :: IO [Integer] -- Read and bind the number and number of times to replicate it
  let numberToSumDigitsOf = read $ concat $ genericReplicate numberOfTimesToReplicate (show number) :: Integer -- Replicate the given number to given number of times and set it
  print $ superDigit numberToSumDigitsOf -- Print the super digit of the given number, replicated the given number of times
