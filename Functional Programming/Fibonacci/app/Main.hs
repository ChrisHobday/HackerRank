module Main (main) where

import Control.Monad ( replicateM )

modification = 100000007

fib 1 = 0
fib 2 = 1
fib n = fib (n - 1) + fib (n - 2)

main :: IO ()
main = do
  numberOfTestCases <- readLn :: IO Int -- Read and bind the number of test cases about to be entered

  fibonacciNumbers <- replicateM numberOfTestCases $ do -- For each test case to be entered...
    readLn :: IO Int -- Read and bind the fibonacci number to find
  
  mapM_ (print . (`mod` modification) .fib) fibonacciNumbers