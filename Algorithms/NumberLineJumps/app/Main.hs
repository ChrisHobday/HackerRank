module Main where

kangaroos :: Int -> Int -> Int -> Int -> IO ()
kangaroos x1 v1 x2 v2
  | (v1 > v2) && (x1 - x2) `mod` (v2 - v1) == 0 = putStrLn "YES"
  | otherwise                                   = putStrLn "NO"

main :: IO ()
main = do
  kangarooInfoTemp <- getLine -- Read line containing kangaroo info
  let kangarooInfo  = map (read :: String -> Int) (words kangarooInfoTemp) -- Convert read line to integers
      x1            = head kangarooInfo
      v1            = kangarooInfo !! 1
      x2            = kangarooInfo !! 2
      v2            = kangarooInfo !! 3
  kangaroos x1 v1 x2 v2
