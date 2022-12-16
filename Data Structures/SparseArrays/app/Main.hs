module Main (main) where

import Control.Monad ( replicateM )

-- The number of matching query occurences in a given list of strings, formatted as a string with a number on each line
matchingStrings :: Eq a => [a] -> [a] -> [Char]
-- No strings
matchingStrings [] _ = ""
-- No queries
matchingStrings _ [] = ""
matchingStrings stringList queries@(q:qs) = show ((length . filter (== q)) stringList) ++ (if qs /= [] then "\n" else "") ++ matchingStrings stringList qs -- Add number of matching query occurences to string and check matching strings on the rest of the queries

main :: IO ()
main = do
  n <- readLn :: IO Int -- Read and bind number of strings to be entered
  stringList <- replicateM n $ do -- For each string to be entered
    getLine -- Get entered string
  q <- readLn :: IO Int -- Read and bind number of queries to be entered
  queries <- replicateM q $ do -- For each query to be entered
    getLine -- Get entered query
  putStrLn $ matchingStrings stringList queries -- Print the number of matching strings between queries and string list
