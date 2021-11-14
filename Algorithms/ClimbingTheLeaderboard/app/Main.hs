module Main where

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

-- The (dense) rank of a given score compared to a given list of ranks
rank :: (Num b, Ord a) => [a] -> a -> b
rank [] _  = 1
rank [rs] score
  | score >= rs = 1
  | otherwise   = 2
rank (rs1:rs2:rss) score
  | score >= rs1 = 1
  | rs1 == rs2   = rank (rs2:rss) score
  | otherwise    = 1 + rank (rs2:rss) score

-- Print the (dense) ranks of a given list of scores compared to a given list of ranks
climbingLeaderboard :: (Foldable t, Ord a) => [a] -> t a -> IO ()
climbingLeaderboard rankedScores playerScores = do
  mapM_ (print . rank rankedScores) playerScores

main :: IO ()
main = do
  _ <- readLn :: IO Int -- Read number of ranked scores to be entered but don't bind it because we don't need it
  rankedScores <- map read . words <$> getLine :: IO [Int] -- Read list of ranked scores
  _ <- readLn :: IO Int -- Read number of player scores to be entered but don't bind it because we don't need it
  playerScores <- map read . words <$> getLine :: IO [Int] -- Read list of player scores
  climbingLeaderboard rankedScores playerScores -- Print the (dense) ranks of a given list of scores compared to a given list of ranks