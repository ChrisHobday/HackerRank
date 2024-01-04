module Main (main) where

import Control.Monad
  ( replicateM )

winOrLose row1Size row2Size row3Size = ""

main :: IO ()
main = do
  numberOfTestCases <- readLn :: IO Int
  testCaseResults <- replicateM numberOfTestCases $ do
    (row1Size : row2Size : row3Size :_) <- (read <$>) . words <$> getLine :: IO [Int] -- Read and bind row sizes
    return $ winOrLose row1Size row2Size row3Size
  print testCaseResults

