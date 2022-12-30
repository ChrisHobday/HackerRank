module Main (main) where

import Data.List ( mapAccumL
                 , sortBy )
-- import Data.
import Data.Tree

-- A data model representing a binary tree
-- data Tree a = Empty
--             | Node a (Tree a) (Tree a)
--   deriving ( Show )

-- The first character, number of occurences of the first character in a given string, and the leftover characters after all occurences of the first character are removed
-- Example: countFirstAndRemove "abbac" = ('a', 2, "bbc")
countFirstAndRemove :: Eq a => [a] -> (a, Int, [a])
countFirstAndRemove string = (firstChar, occurences, concat leftover)
  where firstChar              = head string
        (occurences, leftover) = mapAccumL (\a b -> (a + fromEnum (b == firstChar), if b /= firstChar then [b] else [])) 0 string

-- A list of each unique char in a given string and the number of times it occures
-- Example: countAll "abbac" = [('a',2),('b',2),('c',1)]
countAll :: Eq a => [a] -> [(a, Int)]
countAll string
  | null string = []
  | otherwise   = (char, occurences) : countAll leftover
    where (char, occurences, leftover) = countFirstAndRemove string

-- A list of character frequencies sorted by least to most frequent
-- Example: sortCharFrequencies [('a',2),('b',2),('c',1)] = [('c',1),('a',2),('b',2)]
sortCharFrequencies charFrequencies = sortBy (\(_,a) (_,b) -> compare a b) charFrequencies

-- buildNode x = if 2*x + 1 > 7 then (x, []) else (x, [2*x, 2*x+1])
-- buildNode x = if 2*x + 1 > 7 then (x, []) else (x, [2*x, 2*x+1])

buildNode x = do
  if 2*x + 1 > 7 then
    return (x, [])
  else
    return (x, [2*x, 2*x+1])

constructTree :: [(Char, Int)] -> Tree (Maybe Char, Int)
constructTree ((char, occurences) : (char', occurences') : charFrequencies) = 
  where siblingNodes = [Node (Just char, occurences), Node (Just char', occurences')]

-- constructTree :: [(Char, Int)] -> Tree (Maybe Char, Int)
-- constructTree ((char, occurences) : (char', occurences') : charFrequencies) = constructTree ((Nothing, combinedOccurences) : charFrequencies) (Node (Just char, occurences) Empty Empty) (Node (Just char', occurences') Empty Empty)
--   where combinedOccurences = occurences + occurences'
--         node [] = Empty


-- constructTree :: [(Char, Int)] -> Tree (Maybe Char, Int)
-- constructTree ((char, occurences) : (char', occurences') : charFrequencies) = Node (Nothing, combinedOccurences) (Node (Just char, occurences) Empty Empty) (Node (Just char', occurences') Empty Empty)
--   where combinedOccurences = occurences + occurences'


main :: IO ()
main = do
  string <- getLine
  print $ countAll string
  return ()
