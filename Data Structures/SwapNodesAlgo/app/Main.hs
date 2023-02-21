module Main (main) where

import Control.Monad ( replicateM )
import Data.Tree
import Data.List ( intersperse )
import Debug.Trace

-- A list of tree nodes in reverse order from a given list of sibling pairs without a root node
-- Example: reversedNodeList [[2, 3], [-1, -1], [-1, -1]] =
-- [Node {rootLabel = -1, subForest = []},Node {rootLabel = -1, subForest = []},Node {rootLabel = -1, subForest = []},Node {rootLabel = -1, subForest = []},Node {rootLabel = 3, subForest = []},Node {rootLabel = 2, subForest = []},Node {rootLabel = 1, subForest = []}]
reversedNodeList :: Num a => [[a]] -> [Tree a]
reversedNodeList siblingPairs = (`Node` []) <$> reverse (concat $ [1] : siblingPairs)

-- A tree build from a given list of nodes (lowest depth first)
-- Example: buildTree [Node {rootLabel = -1, subForest = []},Node {rootLabel = -1, subForest = []},Node {rootLabel = -1, subForest = []},Node {rootLabel = -1, subForest = []},Node {rootLabel = 3, subForest = []},Node {rootLabel = 2, subForest = []},Node {rootLabel = 1, subForest = []}] =
-- Node {rootLabel = 1, subForest = [Node {rootLabel = 3, subForest = [Node {rootLabel = -1, subForest = []},Node {rootLabel = -1, subForest = []}]},Node {rootLabel = 2, subForest = [Node {rootLabel = -1, subForest = []},Node {rootLabel = -1, subForest = []}]}]}
buildTree :: (Eq a, Num a) => [Tree a] -> Tree a
-- There are 3 or more nodes
buildTree nodes@(_ : _ : _ : _) = buildTree $ insertSiblings nodes
  where
    -- Inserts the first 2 nodes of a given list into the first node after that is not -1
    insertSiblings :: (Eq a, Num a) => [Tree a] -> [Tree a]
    -- There are 3 or more nodes
    insertSiblings (firstSibling : secondSibling : potentialParent : restOfPotentialParents)
    -- Cannot be parent because either it's not a proper node or it already has children
      | rootLabel potentialParent == -1 || not (null (subForest potentialParent)) = potentialParent : insertSiblings (firstSibling : secondSibling : restOfPotentialParents)
      -- Parent found
      | otherwise                                                                 = potentialParent { subForest = [secondSibling, firstSibling] } : restOfPotentialParents
    -- There are not 3 or more nodes
    insertSiblings nodes' = nodes'
-- There are not 3 or more nodes
buildTree nodes = head nodes -- return the built tree from inside the list

testSiblingPairs = [[2, 3], [-1, -1], [-1, -1]]
testSiblingPairs2 = [[2, 3], [-1, 4], [-1, 5], [-1, -1], [-1, -1]]
testSiblingPairs3 = [[2, 3], [4, -1], [5, -1], [6, -1], [7, 8], [-1, 9], [-1, -1], [10, 11], [-1, -1], [-1, -1], [-1, -1]]
testReversedNodeList = reversedNodeList testSiblingPairs
testReversedNodeList2 = reversedNodeList testSiblingPairs2
testReversedNodeList3 = reversedNodeList testSiblingPairs3
testTree = buildTree testReversedNodeList
testTree2 = buildTree testReversedNodeList2
testTree3 = buildTree testReversedNodeList3

-- A list representing the in order traversal of nodes of a given binary tree (left most to right most)
-- Example: inOrderTraversal (Node 1 [Node 2 [], Node 3 []]) = [2, 1, 3]
inOrderTraversal :: (Eq a, Num a) => Tree a -> [a]
inOrderTraversal (Node id (firstChild : secondChild : _)) = inOrderTraversal firstChild <> [id] <> inOrderTraversal secondChild
inOrderTraversal (Node id (firstChild : _))               = inOrderTraversal firstChild <> [id]
inOrderTraversal (Node id [])
  | id == -1  = []
  | otherwise = [id]

-- Swap the child nodes of a given tree at all depths of a given multiple
-- Example: swap 1 (Node 1 [Node 2 [], Node 3[]]) = 
-- Node {rootLabel 1, subForest = [Node {rootLabel = 2, subForest = []}, Node {rootLabel = 3, subForest = []}]}
swapNodes :: (Eq a, Num a, Eq p, Num p) => p -> Tree a -> Tree a
swapNodes depthMultiple tree = swapNodes' depthMultiple tree
  where
    -- Helper function for counting down the depth as the tree is traversed and tracking the current node
    swapNodes' depthCountdown currentNode
      -- Node does not continue (we're at end of a branch)
      | rootLabel currentNode == -1 = currentNode
      -- The nodes at this depth should be swapped
      | depthCountdown == 1         = currentNode { subForest = reverse (swapNodes' depthMultiple <$> subForest currentNode) } -- Swap the nodes of the current node's children if they need to be and then reverse the current node's children
      -- The nodes at this depth should not be swapped
      | otherwise                   = currentNode { subForest = swapNodes' (depthCountdown - 1) <$> subForest currentNode } -- Swap the nodes of the current node's children if they need to be

main :: IO ()
main = do
  numberOfSiblingPairs <- readLn :: IO Int -- Read and bind number of sibling pairs to be entered
  siblingPairs <- replicateM numberOfSiblingPairs $ do -- For each sibling pair to be entered...
    (read <$>) . words <$> getLine :: IO [Int] -- Read sibling pair

  numberOfSwaps <- readLn :: IO Int -- Read and bind number of swaps to be entered
  swaps <- replicateM numberOfSwaps $ do -- For each swap to be entered...
    readLn :: IO Int -- Read swap
  
  print swaps

  let nodes = reversedNodeList siblingPairs
      binaryTree = buildTree nodes

  putStrLn $ drawTree $ show <$> binaryTree
  print $ inOrderTraversal binaryTree

  -- print $ inOrderTraversal binaryTree

  -- numberOfQueries <- readLn :: IO Int -- Read and bind number of queries to be entered
  -- queries <- replicateM numberOfQueries $ do -- For each query to be entered...
  --   readLn :: IO Int -- Read query

  -- print queries

  return ()
