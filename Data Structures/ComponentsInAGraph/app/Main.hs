module Main (main) where

import qualified Data.Set as S
import Control.Monad ( replicateM )

-- This problem is heaviliy related to sets, in particular disjoint sets and a well known function called union find.
-- Here we import Data.Set for working with sets, and make out own unionFind function for finding the connections
-- between the user entered edges.

-- The set a given node is a member of (returns an singleton set if node is not a member of any of the given sets)
-- Example: memberSet 4 [S.fromList [1,2,3], S.fromList [4,5,6]] = fromList [4,5,6]
memberSet :: Ord t => t -> [S.Set t] -> S.Set t
memberSet node (firstSet : sets)
  | node `S.member` firstSet = firstSet
  | otherwise                = memberSet node sets
memberSet node []            = S.singleton node

-- A list of sets after finding a potential union between them with a given edge
-- Example: unionFind (1, 7) [S.fromList [1,2,3], S.fromList [4,5,6], S.fromList [7,8,9]] = [fromList [1,2,3,7,8,9],fromList [4,5,6]]
unionFind :: Ord a => (a, a) -> [S.Set a] -> [S.Set a]
unionFind (firstNode, secondNode) sets
  -- Both nodes are members of the same set
  | firstNodeSet == secondNodeSet = sets
  | otherwise                     = S.union firstNodeSet secondNodeSet : filter (\set -> set /= firstNodeSet && set /= secondNodeSet) sets
  where firstNodeSet  = memberSet firstNode sets
        secondNodeSet = memberSet secondNode sets

-- All the sets of a given list of edges
-- Example: allSets [[1,5],[1,6],[2,4]] = [fromList [2,4],fromList [1,5,6]]
allSets :: Ord a => [[a]] -> [S.Set a]
allSets edges = go edges []
  where -- Go through the edges finding unions between sets
        go [] sets = sets
        go ((firstNode' : secondNode' : _) : restOfEdges') sets = go restOfEdges' $ unionFind (firstNode', secondNode') sets

-- The smallest and largest components in a given graph/set
-- Example: componentsInGraph [fromList [2,4],fromList [1,5,6]] = (2, 3)
componentsInGraph :: [S.Set a] -> (Int, Int)
componentsInGraph sets = (smallestComponentSize, largestComponentSize)
  where setSizes               = S.size <$> sets
        smallestComponentSize  = minimum setSizes
        largestComponentSize   = maximum setSizes

main :: IO ()
main = do
  numberOfEdges <- (readLn :: IO Int) -- Read and bind number of edges to be entered
  edges <- replicateM numberOfEdges $ do -- Replicate the following action for each edge to be entered
    (read <$>) . words <$> getLine :: IO [Int] -- Read edge as list of ints

  let (smallestComponentSize, largestComponentSize) = componentsInGraph $ allSets edges -- The smallest and largest components of a given list of edges

  putStrLn $ show smallestComponentSize <> " " <> show largestComponentSize -- Print the smallest and largest components