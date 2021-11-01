module Main where

-- The number of times a given number occurs in a given list
numOccurences :: (Eq a, Num b) => a -> [a] -> b
numOccurences _ [] = 0
numOccurences a bs = sum $ map (const 1) $ filter (== a) bs

-- A list of (bird,sighting) pairs with a given number of birds and a given list of sightings
birdSightings :: (Num b) => Int -> [Int] -> [(Int, b)]
birdSightings _ [] = []
birdSightings 0 _  = []
birdSightings a bs = (a, numOccurences a bs) : birdSightings (a - 1) bs

migratoryBirds as = undefined

main :: IO ()
main = do
  birdsTemp <- getLine -- Read line containing bird sightings
  let birds  = map (read :: String -> Int) (words birdsTemp) -- Convert read String to list of Ints
  print $ birds