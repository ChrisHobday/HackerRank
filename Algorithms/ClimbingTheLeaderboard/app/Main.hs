module Main where

import Data.List ( group )

-- -- The (dense) rank of a given score compared to a given list of ranks
-- rank :: (Num b, Ord a) => [a] -> a -> b
-- rank [] _ = 1
-- rank (rankedScore:rankedScores) score
--   | score >= rankedScore = 1
--   | otherwise            = 1 + rank rankedScores score

-- -- Print the (dense) ranks of a given list of scores compared to a given list of ranks
-- climbingLeaderboard :: (Foldable t, Ord a) => [a] -> t a -> IO ()
-- climbingLeaderboard rankedScores playerScores = do
--   mapM_ (print . rank (map head (group rankedScores))) playerScores

-- -- Print the (dense) ranks of a given list of scores compared to a given list of ranks
-- climbingLeaderboard :: (Foldable t, Ord a) => [a] -> t a -> IO ()
-- climbingLeaderboard rankedScores playerScores = do
--   mapM_ (print . rank rankedScores) playerScores

-- ranks :: [Int] -> [Int] -> [Int]
-- ranks [] _ = 1
-- ranks _ [] = []
-- ranks [rs] (ps:pss)
--   | ps >= rs  = 1 : ranks [rs] pss
--   | otherwise = 
-- ranks [rs] [ps]
-- ranks (rs:rss) (ps:pss)
--   | 

-- Remove duplicates from a sorted list
-- Ex. removeDuplicates [1,1,2,3,3] = [1,2,3]
-- removeDuplicates :: Eq a => [a] -> [a]
-- removeDuplicates = map head . group

-- An index for a given starting index and a list
-- Ex. index 1 [a, b, c] = [(1, a), (2, b), (3, c)]
index :: Num i => i -> [a] -> [(i, a)]
index i (a:as) = (i, a) : index (i + 1) as
index _ _      = []

-- A rank for a list of indexed highscores and a score
-- Ex. rank [(1,100),(2,50),(3,10)] 25 = 3
rank :: (Num a, Ord t) => [(a, t)] -> t -> a
-- No indexed highscores
rank [] _ = 1
-- One indexed highscore
rank [indexedHighscore] score
  | score >= snd indexedHighscore = fst indexedHighscore
  | otherwise                     = fst indexedHighscore + 1
-- Two+ indexed highscores
rank (indexedHighscore:indexedHighscores) score
  | score >= snd indexedHighscore = fst indexedHighscore
  | otherwise                     = rank indexedHighscores score

-- Print the (dense) ranks of a given list of scores compared to a given list of ranks
climbingLeaderboard :: (Foldable t, Ord a) => [a] -> t a -> IO ()
climbingLeaderboard rankedScores playerScores = do
  let indexedHighscores = (index 1 . map head . group) rankedScores -- Indexed list of highscores that removes duplicate entries
  mapM_ (print . rank indexedHighscores) playerScores -- Print the (dense) rank of each score

main :: IO ()
main = do
  _ <- readLn :: IO Int -- Read number of ranked scores to be entered but don't bind it because we don't need it
  rankedScores <- map read . words <$> getLine :: IO [Int] -- Read list of ranked scores
  _ <- readLn :: IO Int -- Read number of player scores to be entered but don't bind it because we don't need it
  playerScores <- map read . words <$> getLine :: IO [Int] -- Read list of player scores
  climbingLeaderboard rankedScores playerScores -- Print the (dense) ranks of a given list of scores compared to a given list of ranks