module Main (main) where

import Control.Monad ( replicateM )

-- Whether the first (x, y) pair of a given list has a unique relation (for every subsequent x in the rest of the pairs there is the same y)
-- Example: isUnique [(1, 3), (2, 2), (1, 2)] = False (because there is a different y (2) for the x (1) when y should always be 3 when x is 1)
isUnique :: (Eq a, Eq b) => [(a, b)] -> Bool
-- There is at least two pairs in the given list
isUnique ((x, y) : (x', y') : restOfPairs)
  -- The pair currently being analysed has the same x as the first pair
  | x == x'   = if y == y' then -- The pair currently being analysed has the same y as the first pair (x and y are the same between the pairs, this means there is still a unique relation)
                  isUnique $ (x, y) : restOfPairs -- Continue analysing rest of pairs
                else -- The pair currently being analysed does not have the same y (the unique relation is broken)
                  False
  -- The pair currently being analysed has a different x
  | otherwise = isUnique $ (x, y) : restOfPairs -- Continue analysing rest of pairs
-- There is less than two pairs in the given list
isUnique _ = True

-- Whether a given list of pairs represent a valid function (for each x in a pair there is always the same y)
isFunction :: [(Int, Int)] -> String
-- There is at least one pair
isFunction pairs@(_ : restOfPairs)
  -- The first pair is unique
  | isUnique pairs = isFunction restOfPairs
  -- The first pair is not unique
  | otherwise      = "NO"
-- There are no pairs
isFunction _       = "YES"

main :: IO ()
main = do
  numberOfTestCasesToBeEntered <- readLn :: IO Int -- Read and bind number of test cases to be entered

  testCases <- replicateM numberOfTestCasesToBeEntered $ do -- For each test case to be entered...

    numberOfPairsToBeEntered <- readLn :: IO Int -- Read and bind number of pairs to be entered

    replicateM numberOfPairsToBeEntered $ do -- For each pair to be entered...

      (x : y : _) <- (read <$>) . words <$> getLine :: IO [Int] -- Read and bind the pair to x and y
      return (x, y) -- Return the pair as (x, y)

  mapM_ (putStrLn . isFunction) testCases -- Print whether or not each test case is a function
