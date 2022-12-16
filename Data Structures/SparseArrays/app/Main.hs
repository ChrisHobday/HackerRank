module Main (main) where

import Control.Monad ( replicateM )

matchingStrings stringList queries = ""

main :: IO ()
main = do
  n <- readLn :: IO Int -- Read and bind number of strings to be entered
  stringList <- replicateM n $ do -- For each string to be entered
    getLine -- Get entered string
  q <- readLn :: IO Int -- Read and bind number of queries to be entered
  queries <- replicateM n $ do -- For each query to be entered
    getLine -- Get entered query
  putStrLn $ matchingStrings stringList queries
  