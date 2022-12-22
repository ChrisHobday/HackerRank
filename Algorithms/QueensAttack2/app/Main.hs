module Main where

import Control.Monad ( replicateM )

-- The default position of each of the 8 obstacles (in this case the edge of the chess board) in the order of; right, up, left, down, upRight, upLeft, downLeft, downRight
defaultObstacles n queenX queenY = ( (queenX + spaceRight + 1, queenY)
                                   , (queenX, queenY + spaceUp + 1)
                                   , (queenX - spaceLeft - 1, queenY)
                                   , (queenX, queenY - spaceDown - 1)
                                   , (queenX + spaceUpRight + 1, queenY + spaceUpRight + 1)
                                   , (queenX - spaceUpLeft - 1, queenY + spaceUpLeft + 1)
                                   , (queenX - spaceDownLeft - 1, queenY - spaceDownLeft - 1)
                                   , (queenX + spaceDownRight + 1, queenY - spaceDownRight - 1)
                                   )
  where spaceRight     = n - queenX
        spaceUp        = n - queenY
        spaceLeft      = queenX - 1
        spaceDown      = queenY - 1
        spaceUpRight   = min spaceUp spaceRight
        spaceUpLeft    = min spaceUp spaceLeft
        spaceDownLeft  = min spaceDown spaceLeft
        spaceDownRight = min spaceDown spaceRight

attackableSquares queenX queenY ( (obstacleRightX,obstacleRightY)
                                , (obstacleUpX,obstacleUpY)
                                , (obstacleLeftX, obstacleLeftY)
                                , (obstacleDownX, obstacleDownY)
                                , (obstacleUpRightX, obstacleUpRightY)
                                , (obstacleUpLeftX, obstacleUpLeftY)
                                , (obstacleDownLeftX, obstacleDownLeftY)
                                , (obstacleDownRightX, obstacleDownRightY)
                                )
  = attackableSquaresRight + attackableSquaresUp + attackableSquaresLeft + attackableSquaresDown + attackableSquaresUpRight + attackableSquaresUpLeft + attackableSquaresDownLeft + attackableSquaresDownRight
  where attackableSquaresRight     = obstacleRightX - queenX - 1
        attackableSquaresUp        = obstacleUpY - queenY - 1
        attackableSquaresLeft      = queenX - obstacleLeftX - 1
        attackableSquaresDown      = queenY - obstacleDownY - 1
        attackableSquaresUpRight   = obstacleUpRightX - queenX - 1
        attackableSquaresUpLeft    = obstacleUpLeftY - queenY - 1
        attackableSquaresDownLeft  = queenX - obstacleDownLeftX - 1
        attackableSquaresDownRight = queenY - obstacleDownRightY - 1

-- limits queenX queenY ((ox, oy):obs) (rl, ll, ul, dl, url, drl, lul, ldl)
--   -- Obstacle same row as queen
--   | oy == queenY =
--     case ox > queenX of
--       -- Obstacle right of queen
--       True  -> min rl ox
--       -- Obstacle left of queen
--       False -> max ll ox
--   -- Obstacle same column as queen
--   | ox == queenX =
--     case oy > queenY of
--       -- Obstacle above queen
--       True  -> min ul oy
--       -- Obstacle below queen
--       False -> max ul oy
--   -- Obstacle diagonal to queen
--   | abs (ox - queenX) == abs (oy - queenY)
--     case ox > queenX of
--       -- Obstacle right of queen
--       True  -> case oy > queenY of
--         -- Obstacle above queen
--         True  -> min url (ox + oy / 2)
--         -- Obstacle below queen
--         False ->
--       -- Obstacle left of queen
--       False -> case oy > queenY of
--         -- Obstacle above queen
--         True  ->
--         -- Obstacle below queen
--         False ->


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
    (x:y:_) <- map read . words <$> getLine :: IO [Int]
    return (x, y)
  print $ queensAttack n k qr qc obstacles -- Print number of squares queen can attack with given board length, number of obstacles, queen row/column, and list of obstacles