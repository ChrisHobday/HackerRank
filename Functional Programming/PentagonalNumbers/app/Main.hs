module Main (main) where

import Control.Monad ( replicateM )

-- Note: This recursive solution is too slow to pass some of the test cases

-- The pentagonal number of a given number (the number of dots that can fit in a pentagon where each side's length is a given number)
pentagonalNumber :: (Eq t, Num t) => t -> t
pentagonalNumber 1      = 1
pentagonalNumber number = (number - 1) * 3 + 1 + pentagonalNumber (number - 1)

main :: IO ()
main = do
  testCases <- readLn :: IO Int -- Read and bind number of test cases to be entered
  testCaseNumbers <- replicateM testCases $ do -- Read and bind the test cases
    readLn :: IO Int -- Read, bind and return number to find pentagonal number for
  mapM_ (print . pentagonalNumber) testCaseNumbers -- Print the pentagonal numbers for each of the given numbers
