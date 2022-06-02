module Main where

hurdleRace :: Int -> [Int] -> Int
hurdleRace maximumJumpHeight hurdleHeights = undefined

main :: IO ()
main = do
  maximumJumpHeight <- read . last . words <$> getLine :: IO Int -- Read maximum height the character can jump
  hurdleHeights <- map read . words <$> getLine :: IO [Int] -- Read list of player scores
  print $ hurdleRace maximumJumpHeight hurdleHeights -- Print the number of doses of potion the character must take to be able to jump all of the hurdles