module Main (main) where

import Control.Monad ( replicateM )

-- Note: This FIFO queue is simulated with linked lists

-- A queue with a new value added to the back
enqueue :: a -> [a] -> [a]
enqueue value queue = queue ++ [value]

-- A queue with it's front element removed
dequeue :: [a] -> [a]
dequeue (value : values) = values
dequeue []               = []

-- The evaluation of a list of queries and queue
evaluateQueries :: (Eq a, Num a, Show a) => [[a]] -> [a] -> IO ()
-- The query type is 1
evaluateQueries ((1 : queryValue : _) : queries) queue = evaluateQueries queries $ enqueue queryValue queue
evaluateQueries ((queryType : _) : queries) queue
  -- The query type is 2
  | queryType == 2 = evaluateQueries queries $ dequeue queue
  -- The query type is 3
  | queryType == 3 = do
                       print $ head queue
                       evaluateQueries queries queue
-- There is not a known query type next in the list of given queries
evaluateQueries _ _ = return ()

testQueries = [[1,42],[2],[1,14],[3],[1,28],[3],[1,60],[1,78],[2],[2]]

main :: IO ()
main = do
  numberOfQueries <- (readLn :: IO Int) -- Read and bind number of queries to be entered
  queries <- replicateM numberOfQueries $ do -- Replicate the following action for each query to be entered
    (read <$>) . words <$> getLine :: IO [Int] -- Read and bind query type and value

  evaluateQueries queries [] -- Evaluate queries with empty starting queue
