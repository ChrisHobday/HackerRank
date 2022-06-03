module Main where

import Control.Monad ( replicateM )

-- Read obstacle (row, column)
readObstaclePosition :: IO (Int,Int)
readObstaclePosition = do
  (ir:ic:_) <- map read . words <$> getLine :: IO [Int]
  return (ir, ic)

-- The number of squares a queen can attack with a given board length, number of obstacles, queen row/column, and list of obstacles
queensAttack :: Int -> Int -> Int -> Int -> [(Int, Int)] -> Int
queensAttack n k r_q c_q obstacles = undefined
  -- Write your code here

main :: IO ()
main = do
  (n:k:_) <- map read . words <$> getLine :: IO [Int] -- Read board length and number of obstacles and bind them to n and k respectively
  (qr:qc:_) <- map read . words <$> getLine :: IO [Int] -- Read queen's row and column position and bind them to qr and qc respectively
  obstacles <- replicateM k readObstaclePosition -- Read k obstacle positions
  print $ queensAttack n k qr qc obstacles