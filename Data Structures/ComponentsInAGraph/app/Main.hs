module Main (main) where

import Control.Monad ( replicateM )

-- The smallest and largest components of a given length of edges
-- componentsInGraph :: [[Int]] -> (Int, Int)
componentsInGraph edges = (0, 1)

main :: IO ()
main = do
  numberOfEdges <- (readLn :: IO Int) -- Read and bind number of edges to be entered
  edges <- replicateM numberOfEdges $ do -- Replicate the following action for each edge to be entered
    (read <$>) . words <$> getLine :: IO [Int] -- Read edge as list of ints

  let (smallestComponent, largestComponent) = componentsInGraph edges -- The smallest and largest components of a given list of edges

  putStrLn $ show smallestComponent <> " " <> show largestComponent -- Print the smallest and largest components
