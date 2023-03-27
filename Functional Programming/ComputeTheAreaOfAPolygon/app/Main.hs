module Main (main) where

import Control.Monad ( replicateM )

-- This solution uses the shoelace formula https://en.wikipedia.org/wiki/Shoelace_formula to calculate the area of a simple polygon (represented as a list of points)

area :: Fractional a => [(a, a)] -> a
area (firstPoint : restOfPoints) = abs $ area' (firstPoint : restOfPoints ++ [firstPoint]) / 2
  where 
    area' ((x, y) : (x', y') : restOfPoints) = (x * y' - x' * y) + area' ((x', y') : restOfPoints)
    area' _                                  = 0

main :: IO ()
main = do
  numberOfPointsToBeEntered <- readLn :: IO Int -- Read and bind number of points to be entered

  points <- replicateM numberOfPointsToBeEntered $ do -- For each point to be entered...
    (x : y : _) <- (read <$>) . words <$> getLine :: IO [Float] -- Read and bind the point to x and y
    return (x, y) -- Return the point as (x, y)
  
  print $ area points -- Print the area of entered polygon (as a list of points)