module Main where

designerPdfViewer h word = undefined
  -- Write your code here

main :: IO ()
main = do
  h <- map read . words <$> getLine :: IO [Int] -- Read list of character heights
  word <- getLine -- Read word to find area of rectangle highlight
  print $ designerPdfViewer h word -- Print the area of the rectangle highlight for given character heights and word