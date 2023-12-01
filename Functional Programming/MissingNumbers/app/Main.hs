module Main (main) where

import Data.List
  ( sort
  , nub
  , (\\) )

main :: IO ()
main = do
  _ <- readLn :: IO Int
  ns <- (read <$>) . words <$> getLine :: IO [Int]
  _ <- readLn :: IO Int
  ms <- (read <$>) . words <$> getLine :: IO [Int]

  putStrLn $ unwords $ show <$> sort (nub $ ms \\ ns)
