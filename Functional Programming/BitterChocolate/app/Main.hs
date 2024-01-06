module Main (main) where

import Control.Monad
  ( replicateM )

winOrLose row1Size row2Size row3Size =
  if row1Size == 0 then
    if row2Size == 0 then
      if row3Size == 0 then
        "WIN"
      else
        if row3Size == 1 then
          "LOSE"
        else
          "WIN"
    else
      if row3Size == 1 then
        "WIN"
      else
      


  else


main :: IO ()
main = do
  numberOfTestCases <- readLn :: IO Int
  testCaseResults <- replicateM numberOfTestCases $ do
    (row1Size : row2Size : row3Size :_) <- (read <$>) . words <$> getLine :: IO [Int] -- Read and bind row sizes
    return $ winOrLose row1Size row2Size row3Size
  print testCaseResults

