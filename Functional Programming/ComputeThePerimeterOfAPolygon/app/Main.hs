module Main (main) where

import Control.Monad ( replicateM )

main :: IO ()
main = do
  numberOfPointsToBeEntered <- readLn :: IO Int -- Read and bind number of points to be entered

  points <- replicateM numberOfPointsToBeEntered $ do -- For each point to be entered...
    (x : y : _) <- (read <$>) . words <$> getLine :: IO [Int] -- Read and bind the point to x and y
    return (x, y) -- Return the point as (x, y)
  
  print points
