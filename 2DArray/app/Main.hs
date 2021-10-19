module Main where

import Data.List

ea :: [[Int]]
ea = [[1,2,3,4,5,6],[7,8,9,10,11,12],[13,14,15,16,17,18],[19,20,21,22,23,24],[25,26,27,28,29,30],[31,32,33,34,35,36]]

toHourglass :: [[Int]] -> (Int, Int) -> [Int]
toHourglass a2d (x,y) = take 3 (drop (x - 1) (a2d !! (y - 1))) ++ take 1 (drop x (a2d !! y)) ++ take 3 (drop (x - 1) (a2d !! (y + 1)))

toHourglasses :: [[Int]] -> (Int, Int) -> [[Int]]
toHourglasses a2d (x,y)
  | y >  4    = []
  | x == 4    = toHourglass a2d (x,y) : toHourglasses a2d (1,y + 1)
  | otherwise = toHourglass a2d (x,y) : toHourglasses a2d (x + 1,y)

sumHourglasses :: [[Int]] -> [Int]
sumHourglasses = fmap sum

hourglassSum :: [[Int]] -> Int
hourglassSum a2d = maximum $ sumHourglasses $ toHourglasses a2d (1,1)

main :: IO ()
main = print $ hourglassSum ea
