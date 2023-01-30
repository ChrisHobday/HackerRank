module Main (main) where

import Control.Monad ( replicateM )
import Data.Tree

buildNode value
  | value == -1 = Nothing
  | otherwise   = Just $ Node value []

buildSiblings :: (Eq a, Num a) => [a] -> Int -> [[Int]] -> [Tree a]
buildSiblings [] _ _                        = []
buildSiblings (firstSibling : restSiblings) numberOfCousins siblingPairs
  | firstSibling == (-1) = buildSiblings restSiblings (numberOfCousins -1) siblingPairs
  | otherwise            = Node firstSibling [] : buildSiblings restSiblings (numberOfCousins -1) siblingPairs

-- buildTree :: [[Int]] -> Int -> Tree Int
buildTree nodes@(firstSiblingPair : restOfSiblingPairs) numberOfNodesToBuild = buildSiblings firstSiblingPair
  where nextNumberOfNodesToBuild = length $ filter (/= -1) $ concat $ take numberOfNodesToBuild nodes
buildTree _ _ = undefined
        

-- A list representing the in order traversal of nodes of a given binary tree
-- Example: (Node 1 [Node 2 [], Node 3 []]) = [2, 1, 3]
inOrderTraversal tree = []

-- Swap the nodes of a given tree at a given depth
swapNodes depth tree = tree

testNodeList = [[2, 3], [4, -1], [5, -1], [6, -1], [7, 8], [-1, 9], [-1, -1], [10, 11], [-1, -1], [-1, -1], [-1, -1]]

main :: IO ()
main = do
  numberOfNodes <- readLn :: IO Int -- Read and bind number of nodes to be entered
  nodes <- replicateM numberOfNodes $ do -- For each node to be entered...
    (read <$>) . words <$> getLine :: IO [Int] -- Read nodes

  -- let binaryTree = buildTree nodes

  -- print binaryTree

  -- numberOfQueries <- readLn :: IO Int -- Read and bind number of queries to be entered
  -- queries <- replicateM numberOfQueries $ do -- For each query to be entered...
  --   readLn :: IO Int -- Read query

  -- print queries

  return ()
