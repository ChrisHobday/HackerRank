module Main (main) where

import Control.Monad
  ( replicateM )
import Data.Array
  ( Array
  , listArray
  , (!) )

-- The length of a given pattern, that pattern as an array and a KMP table for a given pattern (used to do a KMP substring search)
-- Ex: kmpPatternLengthPatternArrayTable "abcababc" = (8, array (0, 7) [(0, 'a'),(1, 'b'),(2, 'c'),(3, 'a'),(4, 'b'),(5, 'a'),(6, 'b'),(7, 'c')], array (0, 7) [(0, 0),(1, 0),(2, 0),(3, 1),(4, 2),(5, 1),(6, 2),(7, 3)])
kmpPatternLengthPatternArrayTable :: Eq a => [a] -> (Int, Array Int a, Array Int Int)
kmpPatternLengthPatternArrayTable pat = (patLength, patArray, kmpTable)
  where
    patLength = length pat
    patArray  = listArray (0, patLength - 1) pat
    kmpTable  = listArray (0, patLength - 1) $ 0 : gen 0 1
    -- A KMP table for the given pattern (minus the first 0, as a  list)
    gen j i
      -- The character at given position j in patArray is not the same as character at given position i in patArray
      | (patArray ! j) /= (patArray ! i) =
        if j /= 0
          -- Given j is not 0
          then
            gen 0 i
          --- Given j is 0
          else
            0 : gen j (i + 1)
      -- Otherwise the character at given position j in patArray is the same as character at given position i in patArray
      | otherwise                        = (j + 1) : gen (j + 1) (i + 1)

-- Whether a give pattern exists in a given string in the form of either "YES" or "NO" (using KMP substring search)
-- Ex: kmpSubstringSearch "abcddeffg"  "def" = "YES"
kmpSubstringSearch :: Eq a => [a] -> [a] -> String
kmpSubstringSearch string pat = kmpSubstringSearch' string 0
  where
    (patLength, patArray, kmpTable) = kmpPatternLengthPatternArrayTable pat
    -- Subfunction so that patArray and kmpTable are only computed once
    kmpSubstringSearch' string' j
      -- There was a missmatch and j cannot move further back (there is no more prefix/suffix to check)
      | j == (-1)            = kmpSubstringSearch' string' 0
      -- J is at the end of the pattern (given pattern is a substring of given string)
      | j == patLength       = "YES"
      -- There are no characters in given string to check
      | null string'         = "NO"
      -- The current character matches the current j index of the pattern array
      | char == patArray ! j = kmpSubstringSearch' chars (j + 1)
      -- Otherwise the current character does not match the current j index of the pattern array
      | otherwise            =
        if j == 0
          -- There was no match previously (it's safe to drop the first character)
          then
            kmpSubstringSearch' chars ((kmpTable ! j) - 1)
          -- There was a match previously (we must keep the first character)
          else
            kmpSubstringSearch' string' (kmpTable ! (j  -  1))
      where
        -- The first character and the rest of the characters of the string
        (char : chars) = string'

main :: IO ()
main = do
  testCases <- readLn :: IO Int -- Read and bind the number of test cases to be entered
  subtringMatches <- replicateM testCases $ do -- Replicate the following action the given number of test cases times
    string <- getLine -- Read and bind the text string to search for the given pattern in
    pat <- getLine -- Read and bind the pattern to search the given text string for
    return $ kmpSubstringSearch string pat -- Return whether the given pat exists in the given text string (using kmp substring search)
  mapM_ putStrLn subtringMatches -- Print whether each test cases given pattern exists in each test cases given text string (each on a seperate line)