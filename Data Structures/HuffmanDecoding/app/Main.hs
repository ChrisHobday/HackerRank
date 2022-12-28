module Main (main) where

import Data.List ( mapAccumL )

-- The first character, number of occurences of the first character in a given string, and the leftover characters after all occurences of the first character are removed
-- Example: countFirstAndRemove "abbac" = ('a', 2, "bbc")
countFirstAndRemove string = (firstChar, occurences, concat leftover)
  where firstChar              = head string
        (occurences, leftover) = mapAccumL (\a b -> (a + fromEnum (b == firstChar), if b /= firstChar then [b] else [])) 0 string

-- A list of each unique char in a given string and the number of times it occures
-- Example: countAll "abbac" = [('a',2),('b',2),('c',1)]
countAll string
  | null string = []
  | otherwise   = (char, occurences) : countAll leftover
    where (char, occurences, leftover) = countFirstAndRemove string

charCount string = ()
  -- where firstChar = head string
  --       filteredString = filter (/= firstChar) string
  --       firstCharCount = count

main :: IO ()
main = do
  string <- getLine
  print $ countAll string
  return ()
