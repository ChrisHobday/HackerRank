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

-- A rank for a list of indexed highscores and a score
-- Ex. rank [(1,100),(2,50),(3,10)] 25 = 3
-- rank :: (Num a, Ord t) => [(a, t)] -> t -> a
-- -- No indexed highscores
-- rank [] _ = 1
-- -- One indexed highscore
-- rank [indexedHighscore] score
--   | score >= snd indexedHighscore = fst indexedHighscore
--   | otherwise                     = fst indexedHighscore + 1
-- -- Two+ indexed highscores
-- rank (indexedHighscore:indexedHighscores) score
--   | score >= snd indexedHighscore = fst indexedHighscore
--   | otherwise                     = rank indexedHighscores score

-- Remove duplicates from a sorted list
-- Ex. removeDuplicates [1,1,2,3,3] = [1,2,3]
-- removeDuplicates :: Eq a => [a] -> [a]
-- removeDuplicates = map head . group

-- An index for a given starting index and a list
-- Ex. index 1 [a, b, c] = [(1, a), (2, b), (3, c)]
-- index :: Num i => i -> [a] -> [(i, a)]
-- index i (a:as) = (i, a) : index (i + 1) as
-- index _ _      = []

-- ihs = [(1, 100), (2, 50), (3, 25)]
-- ps  = [5, 25, 50, 120]

-- A rank for a list of indexed highscores and a score and what's left of the indexed highscores
-- Ex. rank [(1,100),(2,50),(3,10)] 25 = 3
-- rank :: (Num a, Ord t) => [(a, t)] -> t -> (a, [(a, t)])
-- No indexed highscores
-- rank [] _ = (1, [])
-- One indexed highscore
-- rank [indexedHighscore] score
--   | score >= snd indexedHighscore = (fst indexedHighscore, [indexedHighscore])
--   | otherwise                     = (fst indexedHighscore + 1, [])
-- Two+ indexed highscores
-- rank (indexedHighscore:indexedHighscores) score
--   | score >= snd indexedHighscore = (fst indexedHighscore, indexedHighscore:indexedHighscores)
--   | otherwise                     = rank indexedHighscores score

-- ranks _ [] = []
-- ranks [] _ = []
-- ranks indexedHighscores (score:scores) = currentRank : ranks restIndexedHighscores scores
--   where (currentRank, restIndexedHighscores) = rank indexedHighscores score

-- ranked = [100, 100, 50, 40, 40, 20, 10]
-- player = [5, 25, 50, 120]

-- A list of ranks for a given list of scores compared against a given indexed list of highscores sorted from lowest to highest
-- Ex. ranks [(3, 25), (2, 50), (3, 100)] [5, 25, 150] = [4, 3, 1]
ranks :: (Num a, Ord b) => [(a, b)] -> [b] -> [a]
ranks _ []      = []
ranks [] [x]    = [1]
ranks [] (ns:nss) = 1 : ranks [] nss
ranks hss@((i,hs):hss') nss@(ns:nss')
    | ns < hs     = (i+1) : ranks hss nss'
    | ns == hs    = i : ranks hss nss'
    | otherwise = ranks hss' nss

-- Print the (dense) ranks of a given list of scores compared to a given list of ranks
climbingLeaderboard :: Ord b => [b] -> [b] -> IO ()
climbingLeaderboard rankedScores playerScores = do
  let indexedHighscores = (reverse . zip [1..] . map head . group) rankedScores -- Indexed list of highscores that removes duplicate entries and reverses order to be lowest score to highest
  mapM_ print (ranks indexedHighscores playerScores) -- Print the (dense) rank of each score

main :: IO ()
main = do
  _ <- readLn :: IO Int -- Read number of ranked scores to be entered but don't bind it because we don't need it
  rankedScores <- map read . words <$> getLine :: IO [Int] -- Read list of ranked scores
  _ <- readLn :: IO Int -- Read number of player scores to be entered but don't bind it because we don't need it
  playerScores <- map read . words <$> getLine :: IO [Int] -- Read list of player scores
  climbingLeaderboard rankedScores playerScores -- Print the (dense) ranks of a given list of scores compared to a given list of ranks