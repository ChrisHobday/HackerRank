module Main (main) where

rev l = undefined

main :: IO ()
main = do
  list <- getContents
  print $ rev list
