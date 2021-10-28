module Main where

-- minBreaks :: Int -> Int -> [Int] -> Int
-- minBreaks _ minBreakCount [] = minBreakCount
-- minBreaks currentMin minBreakCount (score:scores)
--   = fromEnum (score < currentMin) + minBreaks (min score currentMin) minBreakCount scores
  -- | score < currentMin = 1 + minBreaks score minBreakCount scores
  -- | otherwise          = minBreaks currentMin minBreakCount scores

-- maxBreaks :: Int -> Int -> [Int] -> Int
-- maxBreaks _ maxBreakCount [] = maxBreakCount
-- maxBreaks currentMax maxBreakCount (score:scores)
--   = fromEnum (score > currentMax) + maxBreaks (max score currentMax) maxBreakCount scores
  -- | score > currentMax = 1 + maxBreaks score maxBreakCount scores
  -- | otherwise          = maxBreaks currentMax maxBreakCount scores

-- Calculate the total min breaks and max breaks of a given list of scores
minMaxBreaks :: Int -> Int -> Int -> Int -> [Int] -> (Int,Int)
minMaxBreaks _ minBreakCount _ maxBreakCount [] = (minBreakCount,maxBreakCount)
minMaxBreaks currentMin minBreakCount currentMax maxBreakCount (score:scores)
  = (fromEnum (score < currentMin) + fst (minMaxBreaks (min score currentMin) minBreakCount (max score currentMax) maxBreakCount scores)
    ,fromEnum (score > currentMax) + snd (minMaxBreaks (min score currentMin) minBreakCount (max score currentMax) maxBreakCount scores))

-- Print the total min breaks and max breaks of a given list of scores
breakingRecords :: [Int] -> IO ()
breakingRecords scores = do
  let (minBreaks,maxBreaks) = minMaxBreaks (head scores) 0 (head scores) 0 scores
  putStrLn $ show maxBreaks ++ " " ++ show minBreaks

main :: IO ()
main = do
  _ <- readLn :: IO Int -- Read line containing number of scores to be entered but don't bind it cause it's not needed
  scoresTemp <- getLine -- Read line containing scores
  let scores = map (read :: String -> Int) (words scoresTemp) -- Convert string of scores to int list
  breakingRecords scores -- Print total min breaks and max breaks of given list of scores
