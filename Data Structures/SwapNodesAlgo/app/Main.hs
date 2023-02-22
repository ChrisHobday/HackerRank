module Main (main) where

import Control.Monad ( replicateM )
import Data.Tree ( Tree(..) )
import Data.List ( intersperse )

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

-- Preform a given list of swap nodes functions on a given tree printing the in order traversal of the new tree and passing the new tree along to be swapped with the next swap nodes function
swapNodesAndPrintInOrderTraversal :: (Show a, Eq a, Num a) => [Tree a -> Tree a] -> Tree a -> IO ()
swapNodesAndPrintInOrderTraversal [] _ = return ()
swapNodesAndPrintInOrderTraversal (swapNodesFunction : restOfSwapNodesFunctions) tree = do
  let newTree = swapNodesFunction tree -- New tree created by applying next swap nodes function
  putStrLn $ unwords $ show <$> inOrderTraversal newTree -- Print in order traversal of new tree
  swapNodesAndPrintInOrderTraversal restOfSwapNodesFunctions newTree -- Preform next swap and print with new tree

main :: IO ()
main = do
  numberOfSiblingPairs <- readLn :: IO Int -- Read and bind number of sibling pairs to be entered
  siblingPairs <- replicateM numberOfSiblingPairs $ do -- For each sibling pair to be entered...
    (read <$>) . words <$> getLine :: IO [Int] -- Read sibling pair

  numberOfSwaps <- readLn :: IO Int -- Read and bind number of swaps to be entered
  swapNodesFunctions <- replicateM numberOfSwaps $ do -- For each swap to be entered...
    depthMultiple <- readLn :: IO Int -- Read swap depth multiple
    return (swapNodes depthMultiple) -- Return partially applied swapNodes function
  
  let firstTree = buildTree $ reversedNodeList siblingPairs -- The first tree build from list of entered sibling pairs

  swapNodesAndPrintInOrderTraversal swapNodesFunctions firstTree -- Preform swap nodes functions on tree printing the in order traversal of each new tree and passing the new tree along to be swapped with the next swap nodes function
