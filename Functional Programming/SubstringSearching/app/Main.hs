module Main (main) where

import Control.Monad
  ( replicateM )
import Data.Array
  ( Array
  , array
  , listArray
  , bounds
  , (!)
  )

-- A KMP table for a given pattern, the pattern as an array and the lengt of the pattern (used to do a KMP substring search)
-- Ex: kmpTableAndPatternArrayPatternLength "abcababc" = (array (0,7) [(0, 'a'),(1, 'b'),(2, 'c'),(3, 'a'),(4, 'b'),(5, 'a'),(6, 'b'),(7, 'c')], array (0, 7) [(0, 0),(1, 0),(2, 0),(3, 1),(4, 2),(5, 0),(6, 0),(7, 0)])
-- Note: This agorithm does not set j to the value at the previous index in the table being created and so is not as efficient as it could be
kmpTablePatternArrayPatternLength :: Eq a => [a] -> (Array Int a, Array Int Int, Int)
kmpTablePatternArrayPatternLength pat = (patArray, kmpTable, patLength)
  where
    patLength = length pat
    patArray  = listArray (0, patLength - 1) pat
    kmpTable     = listArray (0, patLength - 1) $ 0 : gen 0 1 False
    gen j i previousMatch
      | (patArray ! j) /= (patArray ! i) =
        if previousMatch
          then
            0 : gen 0 (i + 1) False
            -- gen (kmpTable ! (j - 1)) i False
          else
            0 : gen j (i + 1) False
      | otherwise                        = (j + 1) : gen (j + 1) (i + 1) True

-- TODO: Find out why kmpSubStriingSearch "aab" "ab" fails and gives "NO"
kmpSubstringSearch string pat = kmpSubstringSearch' string 0
  where
    (patArray, kmpTable, patLength) = kmpTablePatternArrayPatternLength pat
    -- Subfunction so that patArray and kmpTable are only computed once
    kmpSubstringSearch' (char : chars) j
      -- There was a missmatch and j cannot move further back (there is no more prefix/suffix to check)
      | j == (-1)            = kmpSubstringSearch' chars 0
      -- J is at the end of the pattern (given pattern is a substring of given string)
      | j == patLength = "YES"
      -- The current character matches the current j index of the pattern array
      | char == patArray ! j = kmpSubstringSearch' chars (j + 1)
      -- Otherwise 
      | otherwise            = kmpSubstringSearch' (char : chars) ((kmpTable ! j) - 1)
    -- There are no characters in given string to check
    kmpSubstringSearch' [] j
      -- J is at the end of the pattern (given pattern is a substring of given string)
      | j == patLength = "YES"
      -- Otherwise j did not reach the end of the pattern (given pattern is not a substring of given string)
      | otherwise      = "NO"

main :: IO ()
main = do
  testCases <- readLn :: IO Int -- Read and bind the number of test cases to be entered
  subtringMatches <- replicateM testCases $ do -- Replicate the following action the given number of test cases times
    text <- getLine -- Read and bind the text string to search for the given pattern in
    pat <- getLine -- Read and bind the pattern to search the given text string for
    return $ kmpSubstringSearch text pat -- Return whether the givne pat exists in the given text string (using kmp substring search)
  mapM_ putStrLn subtringMatches -- Print whether each test cases given pattern exists in each test cases given text string (each on a seperate line)