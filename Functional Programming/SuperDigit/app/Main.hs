module Main (main) where

-- A given number's digit summed together recursively until the number is only a single digit
-- Ex: superDigit 1234 = 1
superDigit :: Integral a => a -> a -> a
superDigit number numberOfTimesToReplicate
  -- Remainder is 0
  | remainder == 0 = 9
  -- Otherwise (Remainder is not 0)
  | otherwise      = remainder
  where
    -- The remainder after modding the number multiplied by the number of times to replicate the number by 9
    remainder = (number * numberOfTimesToReplicate) `mod` 9

main :: IO ()
main = do
  (number : numberOfTimesToReplicate : _) <- map read . words <$> getLine :: IO [Integer] -- Read and bind the number and number of times to replicate it
  print $ superDigit number numberOfTimesToReplicate -- Print the super digit of the given number with the number of times to replicate it
