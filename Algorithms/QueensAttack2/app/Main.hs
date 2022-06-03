module Main where

import Control.Monad ( replicateM )

-- Read obstacle (row, column)
readObstaclePosition :: IO (Int,Int)
readObstaclePosition = do
  (ir:ic:_) <- map read . words <$> getLine :: IO [Int]
  return (ir, ic)

-- The number of squares a queen can attack in different directions (without obstacles)
queensAttackUp n qc = n - qc

queensAttackDown qc = qc - 1

queensAttackLeft qr = qr - 1

queensAttackRight n qr = n - qr

queensAttackUpLeft n qr qc = min (queensAttackUp n qc) (queensAttackLeft qr)

queensAttackUpRight n qr qc = min (queensAttackUp n qc) (queensAttackRight n qr)

queensAttackDownLeft n qr qc = min (queensAttackDown qc) (queensAttackLeft qr)

queensAttackDownRight n qr qc = min (queensAttackDown qc) (queensAttackRight n qr)

-- The number of squares a queen can attack with a given board length and queen position (no obstacles)
queensAttackAll :: Int -> Int -> Int -> Int
queensAttackAll n qr qc = queensAttackUp n qc + queensAttackDown qc + 
                          queensAttackLeft qr + queensAttackRight n qr + 
                          queensAttackUpLeft n qr qc + queensAttackUpRight n qr qc + 
                          queensAttackDownLeft n qr qc + queensAttackDownRight n qr qc

-- The number of squares a queen can attack with a given board length, number of obstacles, queen row/column, and list of obstacles
queensAttack :: Int -> Int -> Int -> Int -> [(Int, Int)] -> Int
queensAttack n k qr qc obstacles = undefined
  -- Write your code here

main :: IO ()
main = do
  (n:k:_) <- map read . words <$> getLine :: IO [Int] -- Read board length and number of obstacles and bind them to n and k respectively
  (qr:qc:_) <- map read . words <$> getLine :: IO [Int] -- Read queen's row and column position and bind them to qr and qc respectively
  obstacles <- replicateM k readObstaclePosition -- Read k obstacle positions
  print $ queensAttack n k qr qc obstacles