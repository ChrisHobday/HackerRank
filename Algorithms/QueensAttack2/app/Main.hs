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
                                   , (queenX + spaceDownRight + 1, queenY - spaceDownRight - 1) )
  where spaceRight     = n - queenX
        spaceUp        = n - queenY
        spaceLeft      = queenX - 1
        spaceDown      = queenY - 1
        spaceUpRight   = min spaceUp spaceRight
        spaceUpLeft    = min spaceUp spaceLeft
        spaceDownLeft  = min spaceDown spaceLeft
        spaceDownRight = min spaceDown spaceRight

-- The number of squares a queen can attack
attackableSquares queenX queenY ( (obstacleRightX,obstacleRightY)
                                , (obstacleUpX,obstacleUpY)
                                , (obstacleLeftX, obstacleLeftY)
                                , (obstacleDownX, obstacleDownY)
                                , (obstacleUpRightX, obstacleUpRightY)
                                , (obstacleUpLeftX, obstacleUpLeftY)
                                , (obstacleDownLeftX, obstacleDownLeftY)
                                , (obstacleDownRightX, obstacleDownRightY))
  = attackableSquaresRight + attackableSquaresUp + attackableSquaresLeft + attackableSquaresDown + attackableSquaresUpRight + attackableSquaresUpLeft + attackableSquaresDownLeft + attackableSquaresDownRight
  where attackableSquaresRight     = obstacleRightX - queenX - 1
        attackableSquaresUp        = obstacleUpY - queenY - 1
        attackableSquaresLeft      = queenX - obstacleLeftX - 1
        attackableSquaresDown      = queenY - obstacleDownY - 1
        attackableSquaresUpRight   = obstacleUpRightX - queenX - 1
        attackableSquaresUpLeft    = obstacleUpLeftY - queenY - 1
        attackableSquaresDownLeft  = queenX - obstacleDownLeftX - 1
        attackableSquaresDownRight = queenY - obstacleDownRightY - 1

-- Update the closest obstacles in the queens attack path, relative to the queens position in all 8 directions
updateObstacles _ _ [] finalObstacles = finalObstacles
updateObstacles queenX queenY ((obstacleX, obstacleY):obstacles) ( obstacleRight
                                                                 , obstacleUp
                                                                 , obstacleLeft
                                                                 , obstacleDown
                                                                 , obstacleUpRight
                                                                 , obstacleUpLeft
                                                                 , obstacleDownLeft
                                                                 , obstacleDownRight )
  | obstacleY == queenY =
  -- Obstacle same row as queen
    if obstacleX > queenX then
      -- Obstacle right of queen
      updateObstacles queenX queenY obstacles ( min obstacleRight (obstacleX, obstacleY)
                                              , obstacleUp
                                              , obstacleLeft
                                              , obstacleDown
                                              , obstacleUpRight
                                              , obstacleUpLeft
                                              , obstacleDownLeft
                                              , obstacleDownRight )
      else
      -- Obstacle left of queen
      updateObstacles queenX queenY obstacles ( obstacleRight
                                              , obstacleUp
                                              , max obstacleLeft (obstacleX, obstacleY)
                                              , obstacleDown
                                              , obstacleUpRight
                                              , obstacleUpLeft
                                              , obstacleDownLeft
                                              , obstacleDownRight )
  | obstacleX == queenX =
  -- Obstacle same column as queen
    if obstacleY > queenY then
      -- Obstacle above queen
      updateObstacles queenX queenY obstacles ( obstacleRight
                                              , min obstacleUp (obstacleX, obstacleY)
                                              , obstacleLeft
                                              , obstacleDown
                                              , obstacleUpRight
                                              , obstacleUpLeft
                                              , obstacleDownLeft
                                              , obstacleDownRight )
      else
      -- Obstacle below queen
      updateObstacles queenX queenY obstacles ( obstacleRight
                                              , obstacleUp
                                              , obstacleLeft
                                              , max obstacleDown (obstacleX, obstacleY)
                                              , obstacleUpRight
                                              , obstacleUpLeft
                                              , obstacleDownLeft
                                              , obstacleDownRight )
  | abs (obstacleX - queenX) == abs (obstacleY - queenY) =
  -- Obstacle diagonal to queen
    if obstacleX > queenX then
      -- Obstacle right of queen
      if obstacleY > queenY then
        -- Obstacle above queen
        updateObstacles queenX queenY obstacles ( obstacleRight
                                                , obstacleUp
                                                , obstacleLeft
                                                , obstacleDown
                                                , min obstacleUpRight (obstacleX, obstacleY)
                                                , obstacleUpLeft
                                                , obstacleDownLeft
                                                , obstacleDownRight )
        else
        -- Obstacle below queen
        updateObstacles queenX queenY obstacles ( obstacleRight
                                                , obstacleUp
                                                , obstacleLeft
                                                , obstacleDown
                                                , obstacleUpRight
                                                , obstacleUpLeft
                                                , obstacleDownLeft
                                                , min obstacleDownRight (obstacleX, obstacleY) )
      else
        -- Obstacle left of queen
        if obstacleY > queenY then
          -- Obstacle above queen
          updateObstacles queenX queenY obstacles ( obstacleRight
                                                  , obstacleUp
                                                  , obstacleLeft
                                                  , obstacleDown
                                                  , obstacleUpRight
                                                  , max obstacleUpLeft (obstacleX, obstacleY)
                                                  , obstacleDownLeft
                                                  , obstacleDownRight )
        else
          -- Obstacle below queen
          updateObstacles queenX queenY obstacles ( obstacleRight
                                                  , obstacleUp
                                                  , obstacleLeft
                                                  , obstacleDown
                                                  , obstacleUpRight
                                                  , obstacleUpLeft
                                                  , max obstacleDownLeft (obstacleX, obstacleY)
                                                  , obstacleDownRight )
  -- Otherwise (obstacle isn't in queen's attack path)
  | otherwise = updateObstacles queenX queenY obstacles ( obstacleRight
                                                        , obstacleUp
                                                        , obstacleLeft
                                                        , obstacleDown
                                                        , obstacleUpRight
                                                        , obstacleUpLeft
                                                        , obstacleDownLeft
                                                        , obstacleDownRight )

-- The number of squares a queen can attack with a given board length, number of obstacles, queen x/y, and list of obstacles
-- Ex. queensAttack 4 0 4 4 [] = 9
queensAttack n _ queenX queenY obstacles = attackableSquares queenX queenY $ updateObstacles queenX queenY obstacles $ defaultObstacles n queenX queenY

main :: IO ()
main = do
  (n:k:_) <- map read . words <$> getLine :: IO [Int] -- Read board length and number of obstacles and bind them to n and k respectively
  (qr:qc:_) <- map read . words <$> getLine :: IO [Int] -- Read queen's row and column position and bind them to qr and qc respectively
  obstacles <- replicateM k $ do -- Read k obstacle positions
    (x:y:_) <- map read . words <$> getLine :: IO [Int]
    return (x, y)
  print $ queensAttack n k qr qc obstacles -- Print number of squares queen can attack with given board length, number of obstacles, queen row/column, and list of obstacles