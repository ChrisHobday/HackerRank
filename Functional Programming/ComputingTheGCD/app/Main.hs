module Main (main) where

-- The greatest common denominator of two given numbers
-- Example: gcd' 45 10 = 5
gcd' :: Integral t => t -> t -> t
gcd' x y
  -- If the remainder is 0
  | remainder == 0 = smallerNumber
  -- Otherwise the remainder is not 0
  | otherwise      = gcd' smallerNumber remainder
  where
    smallerNumber    = min x y
    biggerNumber     = max x y
    (div, remainder) = divMod biggerNumber smallerNumber

main :: IO ()
main = do
  (x : y : _) <- (read <$>) . words <$> getLine :: IO [Int] -- Read two numbers to find GCD between and bind them to x and y
  print $ gcd' x y -- Print the GCD of two entered numbers
