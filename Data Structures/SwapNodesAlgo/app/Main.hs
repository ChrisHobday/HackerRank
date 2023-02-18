module Main (main) where

import Control.Monad ( replicateM )
import Data.Tree
import Debug.Trace

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
      | otherwise                                                                 = potentialParent { subForest = [firstSibling, secondSibling] } : restOfPotentialParents
    -- There are not 3 or more nodes
    insertSiblings nodes' = nodes'
-- There are not 3 or more nodes
buildTree nodes = head nodes -- return the built tree from inside the list

testSiblingPairs = [[2, 3], [-1, -1], [-1, -1]]
testSiblingPairs2 = [[2, 3], [-1, 4], [-1, 5], [-1, -1], [-1, -1]]
testReversedNodeList = (\a -> Node a []) <$> reverse (concat $ [1] : testSiblingPairs)
testReversedNodeList2 = (\a -> Node a []) <$> reverse (concat $ [1] : testSiblingPairs2)

-- -- A list representing the in order traversal of nodes of a given binary tree
-- -- Example: (Node 1 [Node 2 [], Node 3 []]) = [2, 1, 3]
-- inOrderTraversal (Node id (firstChild : secondChild : _)) = inOrderTraversal firstChild <> show id <> inOrderTraversal secondChild
-- inOrderTraversal (Node id (firstChild : _)) = inOrderTraversal firstChild <> show id
-- inOrderTraversal (Node id []) = show id

-- -- Swap the nodes of a given tree at a given depth
-- swapNodes depth tree = tree

main :: IO ()
main = do
  numberOfSiblingPairs <- readLn :: IO Int -- Read and bind number of sibling pairs to be entered
  siblingPairs <- replicateM numberOfSiblingPairs $ do -- For each sibling pair to be entered...
    (read <$>) . words <$> getLine :: IO [Int] -- Read nodes

  let nodes = (\a -> Node a []) <$> reverse (concat $ [1] : siblingPairs)
      binaryTree = buildTree nodes

  putStrLn $ drawTree $ show <$> binaryTree

  -- print $ inOrderTraversal binaryTree

  -- numberOfQueries <- readLn :: IO Int -- Read and bind number of queries to be entered
  -- queries <- replicateM numberOfQueries $ do -- For each query to be entered...
  --   readLn :: IO Int -- Read query

  -- print queries

  return ()
