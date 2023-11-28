module Main (main) where

import Control.Monad ( replicateM )

-- The modification (18^8 + 7) to apply after finding the fibonacci number
modification :: Integer
modification = 100000007

-- The list of fibonacci numbers
-- Example: take 5 fibs = [0,1,1,2,3]
fibs :: [Integer]
fibs = 0 : go 0 1
  where go a b = b : go b (a + b)

-- The given nth fibonacci number
-- Example: fib 5 = 5
fib :: Int -> Integer
fib n = fibs !! n

main :: IO ()
main = do
  numberOfTestCases <- readLn :: IO Int -- Read and bind the number of test cases about to be entered

  fibonacciNumbers <- replicateM numberOfTestCases $ do -- For each test case to be entered...
    readLn :: IO Int -- Read and bind the fibonacci number to find
  
  mapM_ (print . (`mod` modification) . fib) fibonacciNumbers -- Print the results of the correct fibonacci number "mod" the modification