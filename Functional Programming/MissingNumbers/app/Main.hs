module Main (main) where

import Data.List
  ( sort
  , nub
  , (\\) )

main :: IO ()
main = do
  _ <- readLn :: IO Int -- Ignore user entered integer
  ns <- (read <$>) . words <$> getLine :: IO [Int] -- Read and bind the first list of user entered integers
  _ <- readLn :: IO Int -- Ignore user entered integer
  ms <- (read <$>) . words <$> getLine :: IO [Int] -- Read and bind the second list of user entered integers

  putStrLn $ unwords $ show <$> sort (nub $ ms \\ ns) -- Print each unqiue integer in the second list but not the first, sorted
