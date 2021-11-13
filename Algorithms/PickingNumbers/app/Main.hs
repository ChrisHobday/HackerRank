module Main where

x = [1,1,2,2,4,4,5,5,5]

pickingNumbers []      = []
pickingNumbers numbers = length (fst subList) : pickingNumbers (snd subList)
                           where subList = span (<= head numbers + 1) numbers

main :: IO ()
main = do
  _ <- readLn :: IO Int
  numbers <- map read . words <$> getLine :: IO [Int]
  print $ maximum $ pickingNumbers numbers
