module Main (main) where

-- A given list reversed
rev :: [a] -> [a]
rev (element : otherElements) = rev otherElements ++ [element]
rev []                        = []
