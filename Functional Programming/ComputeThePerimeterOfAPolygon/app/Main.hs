module Main (main) where

import Control.Monad ( replicateM )

-- The distance between a given two points
-- Example: distanceBetweenTwoPoints (0, 0) (0, 1) = 1
distanceBetweenTwoPoints :: Floating a => (a, a) -> (a, a) -> a
distanceBetweenTwoPoints (x, y) (x', y') = sqrt $ (x' - x)^2 + (y' - y)^2

-- The lengths of lines of a given polygon (distance between each of the points of a polygon in order)
-- Example: lineLengths [(0, 0), (0, 1), (1, 1), (1, 0)] = [1, 1, 1, 1]
lineLengths :: Floating a => [(a, a)] -> [a]
lineLengths (firstPoint : restOfPoints) = lineLengths' (firstPoint : restOfPoints ++ [firstPoint])
  where 
    -- A helper function so that the parent can add the first point to the end in order to get the last line length between the last point and the first point
    lineLengths' (point : point' : restOfPoints) = distanceBetweenTwoPoints point point' : lineLengths' (point' : restOfPoints)
    lineLengths' _                               = []

-- The perimeter of a given polygon (as a list of points)
-- Example: perimeter [(0, 0), (0, 1), (1, 1), (1, 0)] = 4
perimeter :: Floating a => [(a, a)] -> a
perimeter points = sum $ lineLengths points

main :: IO ()
main = do
  numberOfPointsToBeEntered <- readLn :: IO Int -- Read and bind number of points to be entered

  points <- replicateM numberOfPointsToBeEntered $ do -- For each point to be entered...
    (x : y : _) <- (read <$>) . words <$> getLine :: IO [Float] -- Read and bind the point to x and y
    return (x, y) -- Return the point as (x, y)
  
  print $ perimeter points -- Print the perimeter of entered polygon (as a list of points)
