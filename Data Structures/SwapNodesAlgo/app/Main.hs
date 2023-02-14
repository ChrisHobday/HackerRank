module Main (main) where

import Control.Monad ( replicateM )
import Data.Tree

-- Build sibling nodes from a list of number pairs
-- Example: buildSiblingNodes [2, -1] = [Node {rootLabel = 2, subForest = []}]
buildSiblingNodes :: (Eq a, Num a) => [a] -> [Tree a]
buildSiblingNodes [] = []
buildSiblingNodes (firstSibling : restOfSiblings)
  | firstSibling == -1 = buildSiblingNodes restOfSiblings
  | otherwise          = Node firstSibling [] : buildSiblingNodes restOfSiblings

-- Group a list of of child nodes by their depth in the tree
-- Example: groupChildrenByDepth 1 [[Node {rootLabel = 1, subForest = []}],[Node {rootLabel = 2, subForest = []},Node {rootLabel = 3, subForest = []}]] = 
--   [[[Node {rootLabel = 1, subForest = []}]],[[Node {rootLabel = 2, subForest = []},Node {rootLabel = 3, subForest = []}]],[]]
groupChildrenByDepth :: Foldable t => Int -> [t a] -> [[t a]]
groupChildrenByDepth 0 _ = []
groupChildrenByDepth numberOfChildrenNodePairsAtDepth childrenNodePairs = groupedChildren : groupChildrenByDepth numberOfChildrenToGroupNext restOfChildNodePairs
  where (groupedChildren, restOfChildNodePairs) = splitAt numberOfChildrenNodePairsAtDepth childrenNodePairs
        numberOfChildrenToGroupNext             = sum $ length <$> groupedChildren

-- Insert child pairs into their parent nodes
-- Example: insertChildPairs [[Node {rootLabel = 2, subForest = []},Node {rootLabel = 3, subForest = []}]] [[Node {rootLabel = 1, subForest = []}]] =
--   [[Node {rootLabel = 1, subForest = [Node {rootLabel = 2, subForest = []},Node {rootLabel = 3, subForest = []}]}]]
insertChildPairs :: [[Tree a]] -> [[Tree a]] -> [[Tree a]]
insertChildPairs [] _ = []
insertChildPairs _ [] = []
insertChildPairs childPairs (firstParentPair : restOfParentPairs)
  | null firstParentPair = [] : insertChildPairs childPairs restOfParentPairs
  | otherwise            = zipWith (\a b -> b { subForest = a }) childPairsToUse firstParentPair : insertChildPairs restOfChildPairs restOfParentPairs
  where (childPairsToUse, restOfChildPairs) = splitAt (length firstParentPair) childPairs

-- Build a tree from a given list of numberPairs
-- Example: buildTree [[2, 3],[-1, -1],[-1, -1]] = Node {rootLabel = 1, subForest = [Node {rootLabel = 2, subForest = []},Node {rootLabel = 3, subForest = []}]}
buildTree :: (Num a, Eq a) => [[a]] -> Tree a
buildTree siblingNodeList = head $ concat $ foldl insertChildPairs [[]] $ reverse $ groupChildrenByDepth 1 $ [Node 1 []] : (buildSiblingNodes <$> siblingNodeList)

-- A list representing the in order traversal of nodes of a given binary tree
-- Example: (Node 1 [Node 2 [], Node 3 []]) = [2, 1, 3]
inOrderTraversal tree = []

-- Swap the nodes of a given tree at a given depth
swapNodes depth tree = tree

main :: IO ()
main = do
  numberOfNodes <- readLn :: IO Int -- Read and bind number of nodes to be entered
  nodes <- replicateM numberOfNodes $ do -- For each node to be entered...
    (read <$>) . words <$> getLine :: IO [Int] -- Read nodes

  let binaryTree = buildTree nodes

  print binaryTree

  -- numberOfQueries <- readLn :: IO Int -- Read and bind number of queries to be entered
  -- queries <- replicateM numberOfQueries $ do -- For each query to be entered...
  --   readLn :: IO Int -- Read query

  -- print queries

  return ()
