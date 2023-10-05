module Main (main) where

import Control.Monad ( replicateM )

-- The pentagonal number of a given number (the number of dots that can fit in a pentagon where each side's length is a given number)
pentagonalNumber :: Integral a => a -> a
pentagonalNumber number = (3 * number * number - number) `div` 2

main :: IO ()
main = do
  testCases <- readLn :: IO Int -- Read and bind number of test cases to be entered
  testCaseNumbers <- replicateM testCases $ do -- Read and bind the test cases
    readLn :: IO Int -- Read, bind and return number to find pentagonal number for
  mapM_ (print . pentagonalNumber) testCaseNumbers -- Print the pentagonal numbers for each of the given numbers
