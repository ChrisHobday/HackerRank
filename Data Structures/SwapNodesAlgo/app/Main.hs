module Main (main) where

import Control.Monad ( replicateM )
import Data.Tree
import Debug.Trace

-- buildTree :: [Tree Int] -> [Tree Int]
-- buildTree :: (Eq a, Num a) => [Tree a] -> [Tree a]
-- buildTree (firstSibling : secondSibling : potentialParent : restOfPotentialParents)
--   -- Cannot be parent because either it's not a proper node or it already has children
--   | rootLabel potentialParent == -1 || not (null (subForest potentialParent)) = buildTree $ potentialParent : buildTree (firstSibling : secondSibling : restOfPotentialParents)
--   -- Parent found
--   | otherwise                                                                 = potentialParent { subForest = [firstSibling, secondSibling] } : restOfPotentialParents
-- buildTree finalTree = finalTree

insertSiblings (firstSibling : secondSibling : potentialParent : restOfPotentialParents)
  | rootLabel potentialParent == -1 || not (null (subForest potentialParent)) = potentialParent : insertSiblings (firstSibling : secondSibling : restOfPotentialParents)
  | otherwise                                                                 = potentialParent { subForest = [firstSibling, secondSibling] } : restOfPotentialParents
insertSiblings nodes' = nodes'

buildTree nodes@(_ : _ : _ : _) = buildTree $ insertSiblings nodes
buildTree nodes = head nodes

-- siblingPairs = [[2, 3], [-1, -1], [-1, -1]]
-- siblingPairs2 = [[2, 3], [-1, 4], [-1, 5], [-1, -1], [-1, -1]]
-- reversedNodeList = (\a -> Node a []) <$> reverse (concat $ [1] : siblingPairs)
-- reversedNodeList2 = (\a -> Node a []) <$> reverse (concat $ [1] : siblingPairs2)

-- -- Build sibling nodes from a list of number pairs
-- -- Example: buildSiblingNodes [2, -1] = [Node {rootLabel = 2, subForest = []}]
-- buildSiblingNodes :: (Eq a, Num a) => [a] -> [Tree a]
-- buildSiblingNodes [] = []
-- buildSiblingNodes (firstSibling : restOfSiblings)
--   -- | firstSibling == -1 = buildSiblingNodes restOfSiblings
--   | otherwise          = Node firstSibling [] : buildSiblingNodes restOfSiblings

-- -- Group a list of of child nodes by their depth in the tree
-- -- Example: groupChildrenByDepth 1 [[Node {rootLabel = 1, subForest = []}],[Node {rootLabel = 2, subForest = []},Node {rootLabel = 3, subForest = []}]] = 
-- --   [[[Node {rootLabel = 1, subForest = []}]],[[Node {rootLabel = 2, subForest = []},Node {rootLabel = 3, subForest = []}]],[]]
-- groupChildrenByDepth 0 _ = []
-- groupChildrenByDepth numberOfChildrenNodePairsAtDepth childrenNodePairs = groupedChildren : groupChildrenByDepth numberOfChildrenToGroupNext restOfChildNodePairs
--   where (groupedChildren, restOfChildNodePairs) = splitAt numberOfChildrenNodePairsAtDepth childrenNodePairs
--         numberOfChildrenToGroupNext             = sum $ length <$> (filter ((/= -1) . rootLabel) <$> groupedChildren)

-- -- Insert child pairs into their parent nodes
-- -- Example: insertChildPairs [[Node {rootLabel = 2, subForest = []},Node {rootLabel = 3, subForest = []}]] [[Node {rootLabel = 1, subForest = []}]] =
-- --   [[Node {rootLabel = 1, subForest = [Node {rootLabel = 2, subForest = []},Node {rootLabel = 3, subForest = []}]}]]
-- -- insertChildPairs :: [[Tree a]] -> [[Tree a]] -> [[Tree a]]
-- insertChildPairs [] _ = []
-- insertChildPairs _ [] = []
-- insertChildPairs childPairs (firstParentPair : restOfParentPairs)
--   | null firstParentPair = [] : insertChildPairs childPairs restOfParentPairs
--   | otherwise            = zipWith (\a b -> if rootLabel b /= -1 then b { subForest = a } else b) childPairsToUse firstParentPair : insertChildPairs restOfChildPairs restOfParentPairs
--   where (childPairsToUse, restOfChildPairs) = splitAt (length $ filter ((/= -1) . rootLabel) firstParentPair) childPairs

-- -- Build a tree from a given list of numberPairs
-- -- Example: buildTree [[2, 3],[-1, -1],[-1, -1]] = Node {rootLabel = 1, subForest = [Node {rootLabel = 2, subForest = []},Node {rootLabel = 3, subForest = []}]}
-- buildTree :: (Num a, Eq a) => [[a]] -> Tree a
-- buildTree siblingNodeList = head $ concat $ foldl1 insertChildPairs $ reverse $ groupChildrenByDepth 1 $ [Node 1 []] : (buildSiblingNodes <$> siblingNodeList)

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
