module Main (main) where

import Data.List ( mapAccumL
                 , sortBy
                 , lookup )
import Data.Tree ( Tree ( Node ) )
import Data.Maybe ( fromJust )

-- Note: This challenge is poorly worded and designed. By the description it's not clear what is expected input/output should be.
-- In actuality the input is a string of characters and the output is supposed to be the same string of characters, so you could
-- technically beat the challenge by simply outputing the string you get as input. In the spirit of the challenge, here we use the 
-- inputted string to contruct a huffman tree, use that tree to construct a character encoding list, use that list to create an
-- encoding of the original string, and then use that encoding and the tree to decode the original input.

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

-- A list of characters and their binary encoding from a given tree
-- Example: charEncodings (Node (Nothing, 1) [Node (Just 'b', 2) [], Node (Just 'c', 3) []]) = [('b',"0"),('c',"1")]
charEncodings :: Tree (Maybe Char, b) -> [(Char, String)]
charEncodings tree = convert "" tree
  -- A helper function which makes the inputs of charEncodings nicer
  -- A list of characters and their binary encodings, built from a given starting encoding and tree
  -- Example: convert "" (Node (Nothing, 1) [Node (Just 'b', 2) [], Node (Just 'c', 3) []]) = [('b',"0"),('c',"1")]
  where convert :: String -> Tree (Maybe Char, b) -> [(Char, String)]
        -- The tree has both left and right children nodes
        convert code (Node (_, _) (leftNode : rightNode : _)) = (convert (code ++ "0") leftNode ) ++ (convert (code ++ "1") rightNode)
        -- The tree only has a left child node
        convert code (Node (_, _) (leftNode : _))             = (convert (code ++ "0") leftNode )
        -- The tree has no children nodes
        convert code (Node (Just char, _) [])                 = [(char, code)]
        -- The tree has no children nodes and the root node is Nothing (This should not be possible in a huffman tree)
        convert code (Node (Nothing, _) [])                   = undefined -- Error

-- The huffman encoding of a given string
encode :: String -> String
encode string = concat $ encodeCharacter (charEncodings tree) <$> string 
  where tree = constructTree (sortCharFrequencies (countAll string)) Nothing
        -- The huffman encoding of a given character using a given list of character encodings
        encodeCharacter encodings char = fromJust $ lookup char encodings

-- The decoded string of a given code and tree
-- Example: decode "010" (Node (Nothing, 1) [Node (Just 'b', 2) [], Node (Just 'c', 3) []]) = "bcb"
decode :: String -> Tree (Maybe Char, b) -> String
decode code tree = decode' code tree
        -- A helper function so that the initial tree doesn't go out of scope when traversing it
  where decode' :: String -> Tree (Maybe Char, b) -> String
        -- There are no more bits to decode, and the tree has no child nodes (We've arrived at the last character)
        decode' "" (Node (Just char, _) []) = [char]
        -- There are more bits to decode, and the tree has no child nodes (We've arrived at a character)
        decode' code (Node (Just char, _) [])  = char : decode code tree
        -- 
        decode' (bit : bits) (Node (_, _) (leftChild : rightChild : _))
          -- The next bit in the code is 0
          | bit == '0' = decode' bits leftChild -- Continue decoding character with left child
          -- Otherwise (The next bit in the code is 1)
          | otherwise  = decode' bits rightChild -- Continue decoding character with right child
        -- Otherwise (This should not be possible in a huffman tree)
        decode' _ _ = undefined

main :: IO ()
main = do
  string <- getLine -- Read and bind string to encode and decode
  putStrLn $ decode (encode string) (constructTree (sortCharFrequencies (countAll string)) Nothing) -- Print given string encoded and decoded
