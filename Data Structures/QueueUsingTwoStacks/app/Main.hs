module Main (main) where

import Control.Monad ( replicateM )
import qualified Data.Sequence as S

-- Note: This FIFO queue is simulated with a Seq (finger tree from Data.Sequence, which allows fast access to both front and back)

-- A queue with a new value added to the back
enqueue :: a -> S.Seq a -> S.Seq a
enqueue value queue = queue S.|> value

-- A queue with it's front element removed
dequeue :: S.Seq a -> S.Seq a
dequeue (value S.:<| values) = values
dequeue S.Empty              = S.empty

-- The evaluation of a list of queries and queue
evaluateQueries :: (Eq a, Num a, Show a) => [[a]] -> S.Seq a -> IO ()
-- The query type is 1
evaluateQueries ((1 : queryValue : _) : queries) queue = evaluateQueries queries $ enqueue queryValue queue
evaluateQueries ((queryType : _) : queries) queue
  -- The query type is 2
  | queryType == 2 = evaluateQueries queries $ dequeue queue
  -- The query type is 3
  | queryType == 3 = do
                       let (frontValue S.:< _) = S.viewl queue -- The front element of the queue
                       print frontValue
                       evaluateQueries queries queue
-- There is not a known query type next in the list of given queries
evaluateQueries _ _ = return ()

main :: IO ()
main = do
  numberOfQueries <- (readLn :: IO Int) -- Read and bind number of queries to be entered
  queries <- replicateM numberOfQueries $ do -- Replicate the following action for each query to be entered
    (read <$>) . words <$> getLine :: IO [Int] -- Read and bind query type and value

  evaluateQueries queries S.empty -- Evaluate queries with empty starting queue
