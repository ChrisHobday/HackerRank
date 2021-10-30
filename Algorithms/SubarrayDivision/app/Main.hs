module Main where

-- Calculate the number of contiguous segments where the length = Ron's birth month and the sum of the integers on = Ron's birth day
birthday :: [Int] -> Int -> Int -> Int
birthday [] _ _ = 0
birthday s d m
  | sum (take m s) == d = 1 + birthday (tail s) d m
  | otherwise           = birthday (tail s) d m

main :: IO ()
main = do
  _ <- readLn :: IO Int -- Read Line containing number of squares to be entered
  sTemp <- getLine -- Read line containing chocolate squares with integers on
  dMTemp <- getLine -- Read line Ron's birth day and month
  let s  = map (read :: String -> Int) (words sTemp) -- Convert read String to list of Ints
      dM = map (read :: String -> Int) (words dMTemp) -- Convert read String to list of Ints
      d  = head dM -- Set birth day
      m  = last dM -- Set birth month
  print $ birthday s d m -- Print the number of contiguous segments where the length = Ron's birth month and the sum of the integers on = Ron's birth day
