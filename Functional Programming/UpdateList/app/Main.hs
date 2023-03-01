module Main (main) where

-- A list of absolute numbers from a given list of numbers
f :: Num a => [a] -> [a]
f (number : otherNumbers) = abs number : f otherNumbers
f []                      = []

-- This section handles the Input/Output and can be used as it is. Do not modify it.
main = do
  inputdata <- getContents
  mapM_ putStrLn $ map show $ f $ map (read :: String -> Int) $ lines inputdata
