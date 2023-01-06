module Main (main) where

import Data.List ( mapAccumL
                 , sortBy )
import Data.Tree ( Tree ( Node
                        , rootLabel )
                 , drawTree
                 , flatten
                 , foldTree )
import Data.Maybe ( isJust
                  , fromJust )

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
sortCharFrequencies :: Ord a1 => [(a2, a1)] -> [(a2, a1)]
sortCharFrequencies charFrequencies = sortBy (\(_,a) (_,b) -> compare a b) charFrequencies

-- A Huffman Decoding tree constructed from a given list of character, occurence pairs and maybe an existing tree
-- Example: constructTree [('c',1),('a',2),('b',2)] Nothing = Node {rootLabel = (Nothing,5), subForest = [Node {rootLabel = (Just 'b',2), subForest = []},Node {rootLabel = (Nothing,3), subForest = [Node {rootLabel = (Just 'c',1), subForest = []},Node {rootLabel = (Just 'a',2), subForest = []}]}]}
constructTree :: (Num b, Ord b) => [(a, b)] -> Maybe (Tree (Maybe a, b)) -> Tree (Maybe a, b)
-- There is no character frequencies but is a tree
constructTree [] (Just tree)                                                        = tree
-- There is no character frequencies and no tree
constructTree [] Nothing                                                            = Node (Nothing, 0) []
-- There is at least one character frequency and a tree
constructTree ((char, occurences) : charFrequencies) (Just tree@(Node (_, occurences') _))
  -- Given tree should be on left
  | occurences >= occurences'                                                       = constructTree charFrequencies (Just (Node (Nothing, occurences + occurences') [tree, Node (Just char, occurences) []]))
  -- Given tree should be on left
  | otherwise                                                                       = constructTree charFrequencies (Just (Node (Nothing, occurences + occurences') [Node (Just char, occurences) [], tree]))
-- There is at least two character frequencies but no tree
constructTree ((char, occurences) : (char', occurences') : charFrequencies) Nothing = constructTree charFrequencies (Just (Node (Nothing, occurences + occurences') [Node (Just char, occurences) [], Node (Just char', occurences') []]))
-- There is only one given character frequenct but no tree
constructTree ((char, occurences) : _) Nothing                                      = Node (Just char, occurences) []

x = constructTree (sortCharFrequencies $ countAll "ABRACADABRA") Nothing

-- huffmanEncodeChar :: Char -> Tree (Maybe a, b) -> Maybe String
-- huffmanEncodeChar char (Node (_, _) (Node (Just leftChar, _) _ : Node (Just rightChar, _) _ : _)) =

-- huffmanEncodeChar tree char = go tree char
--   where go (Node (char', _) (leftChild : rightChild : _)) =
--           case char' of

-- huffmanEncodeChar (Node (_, _) (Node (Just leftChar, _) _ : Node (Just rightChar, _) _ : _)) char
--   | leftChar == char  = "0"
--   | rightChar == char = "1"

-- charEncodings :: Tree (Maybe a, b) -> [(Char, String)]
-- charEncodings (Node (_, _) (Node (leftChar, _) _ : Node (rightChar, _) _ : _))
--   | isJust leftChar  = "0"
--   | isJust rightChar = "1"

-- charEncodings :: Tree (Maybe a, b) -> [(Char, String)]
charEncodings = convert ""
  where convert :: String -> Tree (Maybe Char, b) -> [(Char, String)]
        convert code (Node (char, _) (leftNode : rightNode : _)) = if isJust char then [(fromJust char, code)] else (convert (code ++ "0") leftNode ) ++ (convert (code ++ "1") rightNode)
        convert code (Node (char, _) (leftNode : _))             = if isJust char then [(fromJust char, code)] else (convert (code ++ "0") leftNode )
        convert code _                                           = []


main :: IO ()
main = do
  string <- getLine
  putStrLn $ drawTree $ fmap show $ constructTree (sortCharFrequencies $ countAll string) Nothing
