module Main (main) where

-- The sum of all odd numbers in a given list
f :: Integral a => [a] -> a
f (number : otherNumbers)
  | odd number  = number + f otherNumbers
  | even number = f otherNumbers
f [] = 0

-- This part handles the Input/Output and can be used as it is. Do not change or modify it.
main = do
	inputdata <- getContents
	putStrLn $ show $ f $ map (read :: String -> Int) $ lines inputdata
