module Main where

-- Sum the top to bottom diagonal of a 2d matrix
sumTBDiagonal :: [[Int]] -> Int
sumTBDiagonal (x:xs) = head x + (sumTBDiagonal $ fmap tail xs)
sumTBDiagonal []     = 0

-- Sum the bottom to top diagonal of a 2d matrix
sumBTDiagonal :: [[Int]] -> Int
sumBTDiagonal (x:xs) = last x + (sumBTDiagonal $ fmap init xs)
sumBTDiagonal []     = 0

-- Find the absolute difference between sum of top to bottom and bottom to top diagonals
diagonalDifference :: [[Int]] -> Int
diagonalDifference xs = abs $ sumTBDiagonal xs - sumBTDiagonal xs

-- Read n lines from the user
readMultipleLinesAsStringArray :: Int -> IO [String]
readMultipleLinesAsStringArray 0 = return []
readMultipleLinesAsStringArray n = do
    line <- getLine
    rest <- readMultipleLinesAsStringArray(n - 1)
    return (line : rest)

main :: IO ()
main = do
  n <- readLn :: IO Int -- Read the number of lines to be read
  mTemp <- readMultipleLinesAsStringArray n -- Read n lines
  let m = map (\x -> map (read :: String -> Int) (words x)) mTemp -- Convert read lines to 2d matrix of integers
  print $ diagonalDifference m -- Print absolute diagonal difference of given 2d matrix
