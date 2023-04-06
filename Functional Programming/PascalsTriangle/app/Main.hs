module Main (main) where

-- Factorial of a given number
-- Example: factorial 3 = 6
factorial :: (Num a, Enum a) => a -> a
factorial n = product [1 .. n]

-- A given row of the pascal triangle starting a a given column (indexing of row/column starts from 0)
-- Example: pascalRow 1 0 = "1 1 "
-- Note: Can run infinitely if given column is 2 or more than given row
pascalRow :: (Show t, Integral t) => t -> t -> String
pascalRow row column
  -- At the end of the pascal row
  | column - 1 == row = ""
  -- Not at the end of the pascal row (more columns to compute)
  | otherwise         = show (factorial row `div` (factorial column * factorial (row - column))) <> " " <> pascalRow row (column + 1)

-- A pascal triangle with a given number of rows
-- Example: pascalTriangle 3 = "1 \n1 1 \n1 2 1 "
-- Note: Can run infinitely if given number of rows is 0 or less
pascalTriangle :: (Show t, Integral t) => t -> String
pascalTriangle 1            = pascalRow 0 0
pascalTriangle numberOfRows = pascalTriangle (numberOfRows - 1) <> "\n" <> pascalRow (numberOfRows - 1) 0

main :: IO ()
main = do
  numberOfRows <- readLn :: IO Int -- Read and bind number of rows
  
  putStrLn $ pascalTriangle numberOfRows -- Print the pascal triangle for the given number of rows
