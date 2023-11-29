module Main (main) where

main :: IO ()
main = do
  _ <- readLn :: IO Int -- Read integer and ignore it
  ns <- (read <$>) . words <$> getLine :: IO [Integer] -- Read list of integers that will be multiplied together to form n
  _ <- readLn :: IO Int -- Read integer and ignore it
  ms <- (read <$>) . words <$> getLine :: IO [Integer] -- Read list of integers that will be multiplied together to form m
  let n = product ns -- The product of the given ns
      m = product ms -- The product of the given ns

  print $ gcd n m `mod` 1000000007 -- Print the greatest common denominator of n and m modulo 1000000007