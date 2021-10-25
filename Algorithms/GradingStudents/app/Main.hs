module Main where

-- Create an infinite list of the multiples of given number
multiples n = fmap (*n) [0..]

-- Calculate the next multiple of 5 for the given number
nextMultipleOf5 n = (n `div` 5 + 1) * 5

-- Round a given grade with Sam's rounding method
roundGrade :: Int -> Int
roundGrade g
  | g < 38                    = g
  | nextMultipleOf5 g - g < 3 = nextMultipleOf5 g
  | otherwise                 = g

-- Round a given list of grades with Sam's rounding method
roundGrades :: [Int] -> [Int]
roundGrades = fmap roundGrade

-- Read given number of lines as a list of strings
readMultipleLinesAsStringArray :: Int -> IO [String]
readMultipleLinesAsStringArray 0 = return []
readMultipleLinesAsStringArray n = do
    line <- getLine
    rest <- readMultipleLinesAsStringArray(n - 1)
    return (line : rest)

main :: IO ()
main = do
  n <- readLn :: IO Int  -- Read number of grades to be entered
  gTemp <- readMultipleLinesAsStringArray n -- Read n lines
  let g = map (read :: String -> Int) gTemp -- Convert read lines to list of numbers
  mapM_ print (roundGrades g) -- Print each of the rounded grades
