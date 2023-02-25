module Main (main) where

-- Splits all possible branches of a given list of chararcter in two
-- Example: branch "___1______1___" = "__1_1____1_1__"
branch :: [Char] -> [Char]
branch (char : char' : char'' : chars)
  | char' == '1' = '1' : '_' : '1' : branch chars
  | otherwise    = char : branch (char' : char'' : chars)
branch chars = chars

-- Continue branching the first encountered branch left and then continue branching the next encountered branch right
-- Example: branchLeft "___1_1______1_1___" = "__1___1____1___1__"
branchLeft :: [Char] -> [Char]
branchLeft (char : char' : chars)
  | char' == '1' = '1' : '_' : branchRight chars
  | otherwise    = char : branchLeft (char' : chars)
  where
    -- Continue branching the first encountered branch right and then continue branching the next encountered branch left
    -- Note: This function is a sub-function of branchLeft because it's not meant to be called by itself
    branchRight :: [Char] -> [Char]
    branchRight (char : char' : chars)
      | char == '1' = '_' : '1' : branchLeft chars
      | otherwise   = char : branchRight (char' : chars)
    branchRight chars = chars
branchLeft chars = chars

-- A list of list of characters representing a tree with base and branches of a given size starting with a given line
-- Example: drawTree 2 "_____________________1_______1_______1_______1_______1_______1_______1_______1______________________" =
-- [ "_____________________1_______1_______1_______1_______1_______1_______1_______1______________________"
-- , "_____________________1_______1_______1_______1_______1_______1_______1_______1______________________"
-- , "____________________1_1_____1_1_____1_1_____1_1_____1_1_____1_1_____1_1_____1_1_____________________"
-- , "___________________1___1___1___1___1___1___1___1___1___1___1___1___1___1___1___1____________________"]
drawTree :: Int -> [Char] -> [[Char]]
drawTree size startLine = drawBase <> [firstBranch] <> drawBranches (size - 1) firstBranch
  where
    drawBase = replicate size startLine

    firstBranch = branch startLine

    drawBranches :: (Eq t, Num t) => t -> [Char] -> [[Char]]
    drawBranches 0 _                = []
    drawBranches numberOfLines line = newLine : drawBranches (numberOfLines - 1) newLine
      where
        newLine = branchLeft line

recursiveTree numberOfIterations
  | numberOfIterations == 1 = drawTree
  where
    numberOfRoots    = 2 ^ (numberOfIterations - 1)
    numberOfBranches = 2 ^ numberOfIterations

main :: IO ()
main = do
  numberOfIterations <- readLn :: IO Int -- Read and bind number of interations

  print $ recursiveTree numberOfIterations -- Print the recursive tree with the entered number of interations