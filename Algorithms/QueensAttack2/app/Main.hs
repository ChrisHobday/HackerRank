module Main where

import Control.Monad ( replicateM )

-- The number of squares a queen can attack in different directions (without obstacles)
queensAttackUp n qc = n - qc
queensAttackDown qc = qc - 1
queensAttackLeft qr = qr - 1
queensAttackRight n qr = n - qr
queensAttackUpLeft n qr qc = min (queensAttackUp n qc) (queensAttackLeft qr)
queensAttackUpRight n qr qc = min (queensAttackUp n qc) (queensAttackRight n qr)
queensAttackDownLeft n qr qc = min (queensAttackDown qc) (queensAttackLeft qr)
queensAttackDownRight n qr qc = min (queensAttackDown qc) (queensAttackRight n qr)

-- The number of squares a queen can attack with queen row/column, list of obstacles, and tuple of attacks in each direction
summedAttacks :: Int -> Int -> [(Int, Int)] -> (Int, Int, Int, Int, Int, Int, Int, Int) -> Int
-- No obstacles
summedAttacks _ _ [] (u, d, l, r, ul, ur, dl, dr) = u + d + l + r + ul + ur + dl + dr
summedAttacks qr qc ((or, oc):obs) obstacleInterference@(u, d, l, r, ul, ur, dl, dr)
  -- Obstacle effects attacks up
  | oc == qc && or > qr           = summedAttacks qr qc obs (min u (or - qr - 1), d, l, r, ul, ur, dl, dr)
  -- Obstacle effects attacks down
  | oc == qc && or < qr           = summedAttacks qr qc obs (u, min d (qr - or - 1), l, r, ul, ur, dl, dr)
  -- Obstacle effects attacks left
  | or == qr && oc < qc           = summedAttacks qr qc obs (u, d, min l (qc - oc - 1), r, ul, ur, dl, dr)
  -- Obstacle effects attacks right
  | or == qr && oc > qc           = summedAttacks qr qc obs (u, d, l, min r (oc - qc - 1), ul, ur, dl, dr)
  -- Obstacle effects attacks up and left
  | oc < qc && or + oc == qr + qc = summedAttacks qr qc obs (u, d, l, r, min ul (or - qr - 1), ur, dl, dr)
  -- Obstacle effects attacks up and right
  | oc > qc && or - qr == oc - qc = summedAttacks qr qc obs (u, d, l, r, ul, min ur (or - qr - 1), dl, dr)
  -- Obstacle effects attacks down and left
  | or - qr == oc - qc            = summedAttacks qr qc obs (u, d, l, r, ul, ur, min dl (qc - oc - 1), dr)
  -- Obstacle effects attacks down and right
  | or + oc == qr + qc            = summedAttacks qr qc obs (u, d, l, r, ul, ur, dl, min dr (oc - qc - 1))
  -- Obstacle doesn't effect attacks
  | otherwise                     = summedAttacks qr qc obs obstacleInterference

-- The number of squares a queen can attack with a given board length, number of obstacles, queen row/column, and list of obstacles
-- Ex. queensAttack 4 0 4 4 [] = 9
queensAttack :: Int -> Int -> Int -> Int -> [(Int, Int)] -> Int
queensAttack n k qr qc obstacles = summedAttacks qr qc obstacles (queensAttackUp n qc, queensAttackDown qc, queensAttackLeft qr, queensAttackRight n qr, queensAttackUpLeft n qr qc, queensAttackUpRight n qr qc, queensAttackDownLeft n qr qc, queensAttackDownRight n qr qc)

main :: IO ()
main = do
  (n:k:_) <- map read . words <$> getLine :: IO [Int] -- Read board length and number of obstacles and bind them to n and k respectively
  (qr:qc:_) <- map read . words <$> getLine :: IO [Int] -- Read queen's row and column position and bind them to qr and qc respectively
  obstacles <- replicateM k $ do -- Read k obstacle positions
    map read . words <$> getLine :: IO [Int]
  print $ queensAttack n k qr qc obstacles -- Print number of squares queen can attack with given board length, number of obstacles, queen row/column, and list of obstacles