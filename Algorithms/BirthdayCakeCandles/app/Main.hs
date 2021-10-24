module Main where

main :: IO ()
main = do
  n <- readLn :: IO Int  -- Read number of candles to be entered
  candlesTemp <- getLine -- Read list of candles
  let candles = map (read :: String -> Int) (words candlesTemp) -- Convert read candle String to integers
  print $ length $ filter (== maximum candles) candles -- Print the number of candles after being filtered to include only the maximum