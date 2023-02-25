module Main (main) where

branch (char : char' : char'' : chars)
  | char' == '1' = '1' : '_' : '1' : branch chars
  | otherwise    = char : branch (char' : char'' : chars)
branch chars = chars

branchLeft (char : char' : chars)
  | char' == '1' = '1' : '_' : branchRight chars
  | otherwise    = char : branchLeft (char' : chars)
branchLeft chars = chars

branchRight (char : char' : chars)
  | char == '1' = '_' : '1' : branchLeft chars
  | otherwise   = char : branchRight (char' : chars)
branchRight chars = chars

recursiveTree numberOfIterations = ""
  where
    numberOfRoots    = 2 ^ (numberOfIterations - 1)
    numberOfBranches = 2 ^ numberOfIterations

main :: IO ()
main = do
  numberOfIterations <- readLn :: IO Int -- Read and bind number of interations

  print $ recursiveTree numberOfIterations -- Print the recursive tree with the entered number of interations