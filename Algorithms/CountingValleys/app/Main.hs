module Main where

import Data.List ( group )

-- The slope of a given Char
slope :: Char -> Int
slope 'U' = 1
slope 'D' = -1
slope _   = 0

-- The number of valleys walked with a given elevation and path
countingValleys :: Int -> [Char] -> Int
countingValleys _ []             = 0
countingValleys elevation (a:as) = fromEnum (newElevation == 0 && a == 'U') + countingValleys newElevation as
                                     where newElevation = elevation + slope a

main :: IO ()
main = do
  steps <- readLn :: IO Int -- Read line containing number of steps
  path <- getLine -- Read line containing path walked
  print $ countingValleys 0 path -- Print the number of valleys
