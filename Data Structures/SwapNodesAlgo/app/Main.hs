module Main (main) where

import Control.Monad ( replicateM_ )
import Data.Tree

-- Read a binary tree with a given amount of nodes and return it
readTree numberOfNodes = do
  return (Node 1 [])

-- A list representing the in order traversal of nodes of a given binary tree
-- Example: (Node 1 [Node 2 [], Node 3 []]) = [2, 1, 3]
inOrderTraversal tree = []

-- Swap the nodes of a given tree at a given depth
swapNodes depth tree = tree

main :: IO ()
main = do
  numberOfNodes <- readLn :: IO Int -- Read and bind number of nodes to be entered
  binaryTree <- readTree numberOfNodes -- Read and bind binary tree
  numberOfQueries <- readLn :: IO Int -- Read and bind number of queries to be entered
  queries <- replicateM_ numberOfQueries $ do -- For each query to be entered...
    readLn :: IO Int -- Read query

  print "SwapNodesAlgo"
