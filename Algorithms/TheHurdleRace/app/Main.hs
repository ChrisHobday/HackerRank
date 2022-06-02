module Main where

-- Print the number of doses of potion the character must take to be able to jump all of the hurdles
-- Ex. hurdleRace 7 [2, 5, 4, 5, 2] = 0
hurdleRace :: Int -> [Int] -> Int
hurdleRace maximumJumpHeight hurdleHeights = max 0 (maximum hurdleHeights - maximumJumpHeight)

main :: IO ()
main = do
  maximumJumpHeight <- read . last . words <$> getLine :: IO Int -- Read maximum height the character can jump
  hurdleHeights <- map read . words <$> getLine :: IO [Int] -- Read list of player scores
  print $ hurdleRace maximumJumpHeight hurdleHeights -- Print the number of doses of potion the character must take to be able to jump all of the hurdles