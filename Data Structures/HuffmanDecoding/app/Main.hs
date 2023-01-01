module Main (main) where

import Data.List ( mapAccumL
                 , sortBy )
-- import Data.
import Data.Tree
import Data.Maybe

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

-- A Huffman Decoding tree constructed from a given list of character, occurence pairs and maybe an existing tree
-- Example: constructTree [('c',1),('a',2),('b',2)] Nothing = Node {rootLabel = (Nothing,5), subForest = [Node {rootLabel = (Just 'b',2), subForest = []},Node {rootLabel = (Nothing,3), subForest = [Node {rootLabel = (Just 'c',1), subForest = []},Node {rootLabel = (Just 'a',2), subForest = []}]}]}
constructTree :: (Num b, Ord b) => [(a, b)] -> Maybe (Tree (Maybe a, b)) -> Tree (Maybe a, b)
-- There is no character frequencies but is a tree
constructTree [] (Just tree)                                                        = tree
-- There is no character frequencies and no tree
constructTree [] Nothing                                                            = Node (Nothing, 0) []
-- There is at least one character frequency and a tree
-- constructTree ((char, occurences) : charFrequencies) (Just tree)@(Node (_, occurences') _) = tree
constructTree ((char, occurences) : charFrequencies) (Just tree@(Node (_, occurences') _))
  -- Given tree should be on left
  | occurences >= occurences' = constructTree charFrequencies (Just (Node (Nothing, occurences + occurences') [tree, Node (Just char, occurences) []]))
  -- Given tree should be on left
  | otherwise                 = constructTree charFrequencies (Just (Node (Nothing, occurences + occurences') [Node (Just char, occurences) [], tree]))
-- There is at least two character frequencies but no tree
constructTree ((char, occurences) : (char', occurences') : charFrequencies) Nothing = constructTree charFrequencies (Just (Node (Nothing, occurences + occurences') [Node (Just char, occurences) [], Node (Just char', occurences') []]))
-- There is only one given character frequenct but no tree
constructTree ((char, occurences) : charFrequencies) Nothing = Node (Just char, occurences) []


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
