module Main where

extraLongFactorials :: (Eq a, Num a) => a -> a
extraLongFactorials n = undefined

main :: IO ()
main = do
  n <- readLn :: IO Int
  print $ extraLongFactorials n