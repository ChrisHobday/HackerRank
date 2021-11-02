module Main where

import Data.List ( sortBy, minimumBy )

numBirdTypes = 5

-- The number of times a given number occurs in a given list
numOccurences :: (Eq a, Num b) => a -> [a] -> b
numOccurences _ [] = 0
numOccurences a bs = sum $ map (const 1) $ filter (== a) bs

-- A list of (bird,sighting) pairs with a given number of birds and a given list of sightings
birdSightings :: (Num b) => Int -> [Int] -> [(Int, b)]
birdSightings _ [] = []
birdSightings 0 _  = []
birdSightings a bs = birdSightings (a - 1) bs ++ [(a, numOccurences a bs)]

-- The bird with the most sightings from a given list of bird sightings
migratoryBirds :: [Int] -> Int
migratoryBirds as = fst $ minimumBy (\(_,a) (_,b) -> compare b a) (birdSightings numBirdTypes as)
-- migratoryBirds as = fst $ head $ sortBy (\(_,a) (_,b) -> compare b a) (birdSightings numBirdTypes as)
-- migratoryBirds as = fst $ last $ filter ((== maximum (map snd (birdSightings numBirdTypes as))) . snd) (birdSightings numBirdTypes as)

main :: IO ()
main = do
  _ <- readLn :: IO Int -- Read Line containing number of bird sightings to be entered
  birdsTemp <- getLine -- Read line containing bird sightings
  let birds  = map (read :: String -> Int) (words birdsTemp) -- Convert read String to list of Ints
  print $ migratoryBirds birds -- Print the bird with the most sightings