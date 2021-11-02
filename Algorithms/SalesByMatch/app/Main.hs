module Main where

import Data.List ( group, sort )

-- The number of sock pairs of a given group sorted list
sockPairs :: Foldable t => [t a] -> Int
sockPairs []    = 0
sockPairs (sock:socks) = ((length sock) `div` 2) + sockPairs socks

-- The number of sock pairs of a given list
sockMerchant :: Ord a => [a] -> Int
sockMerchant socks = sockPairs $ group $ sort socks

main :: IO ()
main = do
  _ <- readLn :: IO Int -- Read line containing number of socks to be entered but don't bind it because we don't need it
  socksTemp <- getLine -- Read line containing list of socks (numbers representing their color)
  let socks = map (read :: String -> Int) (words socksTemp) -- Convert read String to list of Ints
  print $ sockMerchant socks -- Print the number of sock pairs
