module Main (main) where

import Control.Monad ( replicateM )
import Data.Tree

buildNode value
  | value == -1 = Nothing
  | otherwise   = Just $ Node value []

buildSiblingNodes :: (Eq a, Num a) => [a] -> [Tree a]
buildSiblingNodes [] = []
buildSiblingNodes (firstSibling : restOfSiblings)
  | firstSibling == -1 = buildSiblingNodes restOfSiblings
  | otherwise          = Node firstSibling [] : buildSiblingNodes restOfSiblings

-- treeLevels (siblingNodes : restOfSiblingNodes) = take (length siblingNodes) restOfSiblingNodes
-- groupChildrenByDepth :: Int -> [[Tree a]] -> [[[Tree a]]]
groupChildrenByDepth 0 _ = []
groupChildrenByDepth numberOfChildrenNodePairsAtDepth childrenNodePairs = groupedChildren : groupChildrenByDepth numberOfChildrenToGroupNext restOfChildNodePairs
  where (groupedChildren, restOfChildNodePairs) = splitAt numberOfChildrenNodePairsAtDepth childrenNodePairs
        numberOfChildrenToGroupNext             = sum $ length <$> groupedChildren

-- buildTree (deepestChildPairs : nextDeepestChildPairs : restOfDepths) = insertChildPairs deepestChildPairs nextDeepestChildPairs

insertChildPairs [] _ = []
insertChildPairs _ [] = []
insertChildPairs childPairs (firstParentPair : restOfParentPairs)
  | null firstParentPair = [] : insertChildPairs childPairs restOfParentPairs
  | otherwise            = zipWith (\a b -> b { subForest = a }) childPairsToUse firstParentPair : insertChildPairs restOfChildPairs restOfParentPairs
  where (childPairsToUse, restOfChildPairs) = splitAt (length firstParentPair) childPairs

simplifyNodes [] = []
simplifyNodes (Just node : restOfNodes) = node : simplifyNodes restOfNodes
simplifyNodes (Nothing : restOfNodes)   = simplifyNodes restOfNodes

-- A list representing the in order traversal of nodes of a given binary tree
-- Example: (Node 1 [Node 2 [], Node 3 []]) = [2, 1, 3]
inOrderTraversal tree = []

-- Swap the nodes of a given tree at a given depth
swapNodes depth tree = tree

testNodeList = [[2, 3], [4, -1], [5, -1], [6, -1], [7, 8], [-1, 9], [-1, -1], [10, 11], [-1, -1], [-1, -1], [-1, -1]]
testNodeList2 = [[2, 3], [-1, -1], [-1, -1]]

siblingNodes = [Node 1 []] : (buildSiblingNodes  <$> testNodeList)
siblingNodes2 = [Node 1 []] : (buildSiblingNodes  <$> testNodeList2)

[firstTest, secondTest, thirdTest] = take 3 $ reverse $ groupChildrenByDepth 1 siblingNodes

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
