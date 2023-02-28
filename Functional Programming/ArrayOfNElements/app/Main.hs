module Main (main) where

fn n = undefined

main = do
  n <- readLn :: IO Int
  print (fn(n))
