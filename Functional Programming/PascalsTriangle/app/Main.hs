module Main (main) where

pascalTriangle numberOfRows = undefined

main :: IO ()
main = do
  numberOfRows <- readLn :: IO Int -- Read and bind number of rows
  
  pascalTriangle numberOfRows -- Print the pascal triangle for the given number of rows
