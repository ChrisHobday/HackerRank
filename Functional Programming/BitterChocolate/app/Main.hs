module Main (main) where

import Control.Monad
  ( replicateM )

data ChocolateBar =
  ChocolateBar
    { row1Length :: Int
    , row2Length :: Int
    , row3Length :: Int }
  deriving ( Show )

move chocolateBar x y = ChocolateBar { row1Length = min (row1Length chocolateBar) x - 1
                                     , row2Length = if y < 3 then min (row2Length chocolateBar) x - 1 else row2Length chocolateBar
                                     , row3Length = if y < 2 then min (row3Length chocolateBar) x - 1 else row3Length chocolateBar }

rowMoves 1 1 = []
rowMoves _ 0 = []
rowMoves rowNumber rowLength = (rowLength, rowNumber) : rowMoves rowNumber (rowLength - 1)

possibleMoves chocolateBar = rowMoves 1 (row1Length chocolateBar) <> rowMoves 2 (row2Length chocolateBar) <> rowMoves 3 (row3Length chocolateBar)

-- checkMoves chocolateBar = 

-- winOrLose row1Size row2Size row3Size =
--   if row1Size == 0 then
--     if row2Size == 0 then
--       if row3Size == 0 then
--         "WIN"
--       else
--         if row3Size == 1 then
--           "LOSE"
--         else
--           "WIN"
--     else
--       if row3Size == 1 then
--         "WIN"
--       else
      


--   else


main :: IO ()
main = do
  -- numberOfTestCases <- readLn :: IO Int
  -- testCaseResults <- replicateM numberOfTestCases $ do
  --   (row1Size : row2Size : row3Size :_) <- (read <$>) . words <$> getLine :: IO [Int] -- Read and bind row sizes
  --   return $ winOrLose row1Size row2Size row3Size
  -- print testCaseResults
  return ()
