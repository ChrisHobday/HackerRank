module Main where

-- Calculate the lowest common multiple of a list of numbers
lcm' :: [Int] -> Int
lcm' = foldr lcm 1

-- Calculate the greatest common divisor of a list of numbers
gcd' :: [Int] -> Int
gcd' []     = 1
gcd' (a:as) = foldr gcd a as

-- Calculate the multiples from a given a to a given b
multiplesFromTo :: Int -> Int -> [Int]
multiplesFromTo a b = fmap (* a) [1..(b `div` a)]

-- Calculate the number of given as that divide evenly into given n
dividesEvenly :: [Int] -> Int -> Int
dividesEvenly as b = length $ filter (== 0) (fmap (b `mod`) as)

-- Calculate the number of multiples from lcm of first array to gcd of second array that divide evenly into gcd of second array
getTotalX a b = do
  let aLCM = lcm' a
      bGCD = gcd' b
  print $ dividesEvenly (multiplesFromTo aLCM bGCD) bGCD

main :: IO ()
main = do
  _ <- getLine -- Read line containing array sizes to be entered but don't bind it as we don't need it
  asTemp <- getLine -- Read line containing first array of numbers
  bsTemp <- getLine -- Read line containing first array of numbers
  let as = map (read :: String -> Int) (words asTemp) -- Convert read line to array of numbers
      bs = map (read :: String -> Int) (words bsTemp) -- Convert read line to array of numbers
  getTotalX as bs