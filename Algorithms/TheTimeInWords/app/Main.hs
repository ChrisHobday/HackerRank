module Main (main) where

timeInWords :: Int -> Int -> String
timeInWords hour minutes = "undefined"

main :: IO ()
main = do
  hour <- readLn :: IO Int
  minutes <- readLn :: IO Int
  print $ timeInWords hour minutes
