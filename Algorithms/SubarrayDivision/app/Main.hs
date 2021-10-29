module Main where

import Data.List ( group, sort, subsequences )

-- Remove duplicate occurences in a given list
removeDups :: Ord a => [a] -> [a]
removeDups = map head . group . sort

main :: IO ()
main = do
  putStrLn ""
