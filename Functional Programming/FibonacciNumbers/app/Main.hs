module Main (main) where

-- The fibonacci number at the given position
-- Example: fib 3 = 1
fib 1 = 0
fib 2 = 1
fib n = fib (n - 1) + fib (n - 2)

main :: IO ()
main = do
  nFibonacciNumber <- readLn :: IO Int -- Read and bind the nth fibonacci number to calculate
  
  print $ fib nFibonacciNumber -- Print the nth fibonacci number to calculate
