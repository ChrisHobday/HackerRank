module Main (main) where

recursiveTree numberOfIterations = ""

main :: IO ()
main = do
  numberOfIterations <- readLn :: IO Int -- Read and bind number of interations

  print $ recursiveTree numberOfIterations -- Print the recursive tree with the entered number of interations