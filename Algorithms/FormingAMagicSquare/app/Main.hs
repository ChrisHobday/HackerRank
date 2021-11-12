module Main where

import Data.List ( transpose )

-- A starting 3x3 magic square
startMagicSquare :: [[Int]]
startMagicSquare = [[2,7,6],[9,5,1],[4,3,8]]

-- A 90 degree rotation of a given square
rotate90 :: [[a]] -> [[a]]
rotate90 = map reverse . transpose

-- A reflection of a given square
reflect :: [[a]] -> [[a]]
reflect = transpose

-- The rotated 3x3 magic squares
rotatedMagicSquares :: [[[Int]]]
rotatedMagicSquares = take 4 $ iterate rotate90 startMagicSquare

-- The reflected 3x3 magic squares
reflectedMagicSquares :: [[[Int]]]
reflectedMagicSquares = map reflect rotatedMagicSquares

-- All 3x3 magic squares
allMagicSquares :: [[[Int]]]
allMagicSquares = rotatedMagicSquares ++ reflectedMagicSquares

-- The cost of converting a given square to another given square
cost :: [[Int]] -> [[Int]] -> Int
cost s1 s2 = sum $ map abs $ zipWith (-) (concat s1) (concat s2)

-- The minimum cost of converting a given sqaure to a magic square
formingMagicSquare :: [[Int]] -> Int
formingMagicSquare square = minimum $ map (cost square) allMagicSquares

-- Read a given number of lines
getLines :: Int -> IO [String]
getLines 0 = return []
getLines n = do
    line <- getLine
    rest <- getLines (n - 1)
    return (line : rest)

main :: IO ()
main = do
  squareTemp <- getLines 3 -- Read in square as string
  let square = map (map (read :: String -> Int) . words) squareTemp -- Convert square to list of list of ints
  print $ formingMagicSquare square -- Print the minimum cost of converting given square to magic square
