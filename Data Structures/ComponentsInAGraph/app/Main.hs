module Main (main) where


-- import qualified Data.Set as S
import Control.Monad ( replicateM )
import Data.List ( union
                 , nub )

-- Whether two lists of elements are connected (contain at least 1 of the same element)
-- Example: areConnected [1,2,3] [4,5,1] = True
areConnected :: (Foldable t, Eq a) => [a] -> t a -> Bool
areConnected firstList@(firstElement : elements) secondList
  -- If the first element is an element in the second list
  | firstElement `elem` secondList = True
  | otherwise                      = areConnected elements secondList
areConnected [] _ = False

connectFirst []     = []
connectFirst [lists] = [lists]
connectFirst lists = connectFirst' lists
  where connectFirst' []     = []
        connectFirst' [lists'] = [lists']
        connectFirst' (firstList' : secondList' : lists')
          | firstList' `areConnected` secondList' = connectFirst' $ union firstList' secondList' : lists'
          | otherwise                             = connectFirst' (firstList' : lists') ++ [secondList']

connectAll []    = []
connectAll lists = firstConnected : connectAll restLists
  where (firstConnected : restLists) = connectFirst lists

-- The smallest and largest components of a given length of edges
-- componentsInGraph :: [[Int]] -> (Int, Int)
componentsInGraph edges = nub <$> connectAll edges --(smallestComponentSize, largestComponentSize)
  where componentSizes        = length . nub <$> connectAll edges
        smallestComponentSize = minimum componentSizes
        largestComponentSize  = maximum componentSizes

test = [[1, 6],[2, 7], [3, 8], [4, 9], [2, 6]]
-- edge1 = S.fromList [1, 6]
-- edge2 = S.fromList [2, 7]
-- edge3 = S.fromList [2, 6]

main :: IO ()
main = do
  numberOfEdges <- (readLn :: IO Int) -- Read and bind number of edges to be entered
  edges <- replicateM numberOfEdges $ do -- Replicate the following action for each edge to be entered
    (read <$>) . words <$> getLine :: IO [Int] -- Read edge as list of ints

  --let (smallestComponentSize, largestComponentSize) = componentsInGraph edges -- The smallest and largest components of a given list of edges

  --putStrLn $ show smallestComponentSize <> " " <> show largestComponentSize -- Print the smallest and largest components

  return ()