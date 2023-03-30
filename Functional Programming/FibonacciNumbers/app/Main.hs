module Main (main) where

fib n = undefined

main :: IO ()
main = do
  nFibonacciNumber <- readLn :: IO Int -- Read and bind the nth fibonacci number to calculate
  
  print $ fib nFibonacciNumber -- Print the nth fibonacci number to calculate
