module Main (main) where

import Data.List ( nub )

main :: IO ()
main = do
  string <- getLine -- Read and bind string of characters to remove duplicates from
  putStrLn $ nub string -- Print given string of characters with duplicates removed
