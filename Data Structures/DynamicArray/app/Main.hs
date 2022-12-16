module Main where

import Data.Vector as V
import Data.List as L
import Data.Bits ( xor, Bits )
import Control.Monad  as M( replicateM )

-- An array of n arrays
arrays n = V.replicate n V.empty

idx :: (Integral a, Data.Bits.Bits a) => a -> a -> a -> a
idx x n lastAnswer = (x `xor` lastAnswer) `mod` n

query1 :: Int -> a -> Int -> Int -> Vector (Vector a) -> Vector (Vector a)
query1 x y n lastAnswer arr = arr1 V.++ V.singleton (V.head arr2 V.++ V.singleton y) V.++ V.tail arr2
  where (arr1, arr2) = V.splitAt (idx x n lastAnswer) arr

query2 :: Int -> Int -> Int -> Int -> Vector (Vector a) -> a
query2 x y n lastAnswer arr = arr ! idx' ! (y `mod` V.length (arr ! idx'))
  where idx' = idx x n lastAnswer

-- Evaluates a list of queries of either type 1 or 2
queryEvaluation :: Int -> Int -> Vector (Vector Int) -> Vector (Vector Int) -> [Char]
queryEvaluation n lastAnswer arr queries
  -- There are no queries
  | V.null queries = ""
  -- Query type 1
  | q == 1 = queryEvaluation n lastAnswer (query1 x y n lastAnswer arr) (V.tail queries) -- Update arr and continue on to next query
  -- Query type 2
  | q == 2 = show (query2 x y n lastAnswer arr) L.++ (if not (V.null (V.tail queries)) then "\n" else "") L.++ queryEvaluation n (query2 x y n lastAnswer arr) arr (V.tail queries) -- Append number to string, update lastAnswer and continue to next query
  -- Otherwise (queries are formatted incorrectly)
  | otherwise = ""
    where q = V.head (V.head queries)
          x = V.head queries ! 1
          y = V.head queries ! 2

dynamicArray :: Int -> Vector (Vector Int) -> [Char]
dynamicArray n queries = queryEvaluation n 0 (arrays n) queries

main :: IO ()
main = do
  (n:q:_) <- L.map read . words <$> getLine :: IO [Int] -- Get and bind size of starting array and number of queries
  queries <- V.replicateM q $ do -- For each query
    query <- L.map read . words <$> getLine :: IO [Int] -- Get query type, x and y
    return (V.fromList query) -- Return given query as vector
  putStrLn $ dynamicArray n queries -- Print the results of dynamicArray with given starting array size and queries
