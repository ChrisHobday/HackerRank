module Main where

import Data.List ( sum, transpose, group, sort )

x = [[1,2,3],[4,5,6],[7,8,9]] :: [[Int]]

-- The sum of a given square's rows
rowSums :: Num a => [[a]] -> [a]
rowSums as = map sum as

-- The sum of a given square's columns
columnSums :: Num a => [[a]] -> [a]
columnSums ([]:_)     = []
columnSums as         = foldr ((+) . head) 0 as : columnSums (map tail as)

-- The sum of a given square's diagonal staring at the top
topDiagonalSum :: Num a => [[a]] -> a
topDiagonalSum (a:as) = head a + (topDiagonalSum $ fmap tail as)
topDiagonalSum []     = 0

-- The sum of a given square's diagonal staring at the bottom
bottomDiagonalSum :: Num a => [[a]] -> a
bottomDiagonalSum (a:as) = last a + (bottomDiagonalSum $ fmap init as)
bottomDiagonalSum []     = 0

-- The sums of a given square's columns, rows, and diagonals
squareSums :: Num a => [[a]] -> [a]
squareSums as = rowSums as ++ columnSums as ++ [topDiagonalSum as] ++ [bottomDiagonalSum as]

-- The most frequent occurence in a list
mostFrequent :: Ord a => [a] -> a
mostFrequent as = snd (maximum [ (length bs, head bs) | bs <- group $ sort as ])

-- The magical constant of a given magic square
magicConstant :: (Ord a, Num a) => [[a]] -> a
magicConstant as = mostFrequent $ squareSums as

-- A fixed square number and the amount needed to fix it for a given broken number and magic constant
fixNumber :: Num a => a -> a -> (a, a)
fixNumber = undefined

formingMagicSquare = undefined

main :: IO ()
main = do
  putStrLn ""
