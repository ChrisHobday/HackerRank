module Main where

-- Create the empty part of a step with given depth
createStepEmpty :: Int -> String
createStepEmpty d
  | d /= 0    = " " ++ createStepEmpty (d - 1)
  | otherwise = ""

-- Create the solid part of a step with given depth
createStepSolid :: Int -> String
createStepSolid d
  | d /= 0    = "#" ++ createStepSolid (d - 1)
  | otherwise = ""

-- Create a step with given empty and solid depths
createStep :: Int -> Int -> String
createStep d d' = createStepEmpty d ++ createStepSolid d' ++ "\n"

-- Create a staircase with given height and starting step depth
createStairCase :: Int -> Int -> String
createStairCase h d
  | h /= 0    = createStep h d ++ createStairCase (h - 1) (d + 1)
  | otherwise = createStep h d

main :: IO ()
main = do
  n <- readLn :: IO Int  -- Read height of staircase to be create
  putStr $ createStairCase (n - 1) 1 -- Print a staircase of given height and starting step depth
