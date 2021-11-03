module Main where

-- The minimum number of pages to turn given the number of pages and page to turn to
pageCount nPages page = min (page `div` 2) (nPages `div` 2 - page `div` 2)
  -- | (nPages `div` 2) >= page = page `div` 2
  -- | otherwise                = (nPages `div` 2) - (page `div` 2)


main :: IO ()
main = do
  nPages <- readLn :: IO Int -- Read number of pages
  page <- readLn :: IO Int -- Read page to turn to
  print $ pageCount nPages page -- Print the minimum number of pages to turn
