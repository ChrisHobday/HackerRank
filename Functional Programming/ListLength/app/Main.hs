module Main (main) where

-- The number of elements in a given list
len :: [a] -> Int
len (element : otherElements) = 1 + len otherElements
len []                        = 0
