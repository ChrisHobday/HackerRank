module Main (main) where

import Control.Monad
  ( replicateM
  , mapM_ )

count _ 0 = 1
count n k
  | n == k    = 1
  | n < k     = 1
  | otherwise = count (n - 1) (k - 1) + count (n - 1) k

main :: IO ()
main = do
  testCases <- readLn :: IO Int

  counts <- replicateM testCases $ do
    (n : k : _) <- (read <$>) . words <$> getLine :: IO [Int]
    return (count n k)
  
  mapM_ print counts
