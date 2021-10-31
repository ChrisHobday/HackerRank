module Main where

-- All unique pairs for the first element of a given list
firstPairs :: [a] -> [(a,a)]
firstPairs []     = []
firstPairs (_:[]) = []
firstPairs as = firstPairs (init as) ++ [(head as,last as)]

-- All unique pairs for each element of a given list
allPairs :: [a] -> [(a,a)]
allPairs []     = []
allPairs (_:[]) = []
allPairs as = firstPairs as ++ allPairs (tail as)

-- If given pair is divisible
checkDivisible :: Int -> (Int,Int) -> Bool
checkDivisible a (b,c) = (b + c) `mod` a == 0

-- The number of pairs in a given list of numbers that when summed are divisible by a given number
divisibleSumPairs :: Int -> [Int] -> Int
divisibleSumPairs k ar = sum $ map fromEnum $ map (checkDivisible k) (allPairs ar)

main :: IO ()
main = do
  nkTemp <- getLine -- Read line containing size of array and number to be divisible by
  arTemp <- getLine -- Read line containing array of integers
  let nk = map (read :: String -> Int) (words nkTemp) -- Convert read String to list of Ints
      k  = last nk -- Set k
      ar = map (read :: String -> Int) (words arTemp) -- Convert read String to list of Ints
  print $ divisibleSumPairs k ar -- Print the divisible sum pairs with given k and ar