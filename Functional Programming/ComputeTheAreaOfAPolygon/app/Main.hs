module Main (main) where

import Control.Monad ( replicateM )

-- area = 1/2 perimeter * apothem
area = undefined

main :: IO ()
main = do
  numberOfPointsToBeEntered <- readLn :: IO Int -- Read and bind number of points to be entered

  points <- replicateM numberOfPointsToBeEntered $ do -- For each point to be entered...
    (x : y : _) <- (read <$>) . words <$> getLine :: IO [Float] -- Read and bind the point to x and y
    return (x, y) -- Return the point as (x, y)
  
  print $ area points -- Print the area of entered polygon (as a list of points)
