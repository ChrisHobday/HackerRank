module Main where

-- The factorial of a given number (workds for extra long numbers)
-- Ex. extraLongFactorials 50 = 30414093201713378043612608166064768844377641568960512000000000000
extraLongFactorials :: (Eq a, Num a) => a -> a
extraLongFactorials 0 = 1
extraLongFactorials n = n * extraLongFactorials (n - 1)

main :: IO ()
main = do
  n <- readLn :: IO Integer -- Read a number (stored with Integer type for arbitrary length)
  print $ extraLongFactorials n -- Print the factorial of read number