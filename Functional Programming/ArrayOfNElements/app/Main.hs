module Main (main) where

-- A list of numbers from 0 to 1 less than a given number
fn :: (Num a, Enum a) => a -> [a]
fn n = [0 .. n - 1]

main :: IO ()
main = do
  n <- readLn :: IO Int
  print (fn(n))
