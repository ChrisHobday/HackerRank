module Main where

-- import Data.Vector as V
import Data.Bits ( xor, Bits )
import Control.Monad ( replicateM )

arrays :: Int -> [[a]]
arrays n = replicate n []

idx :: (Integral a, Data.Bits.Bits a) => a -> a -> a -> a
idx x n lastAnswer = (x `xor` lastAnswer) `mod` n

query1 :: Int -> a -> Int -> Int -> [[a]] -> [[a]]
query1 x y n lastAnswer arr = arr1 ++ [ele ++ [y]] ++ arr2
  where (arr1, ele:arr2) = splitAt (idx x n lastAnswer) arr

query2 :: Int -> Int -> Int -> Int -> [[a]] -> a
query2 x y n lastAnswer arr = arr !! idx' !! (y `mod` length (arr !! idx'))
  where idx' = idx x n lastAnswer

-- Evaluates a list of queries of either type 1 or 2
queryEvaluation :: Int -> Int -> [[Int]] -> [[Int]] -> [Char]
-- There are no queries
queryEvaluation _ _ _ [] = ""
-- There is at least 1 query properly formatted
queryEvaluation n lastAnswer arr ((q:x:y:_):queries)
  -- Query type 1
  | q == 1 = queryEvaluation n lastAnswer (query1 x y n lastAnswer arr) queries -- Update arr and continue on to next query
  -- Query Type 2
  | q == 2 = show (query2 x y n lastAnswer arr) ++ (if queries /= [] then "\n" else "") ++ queryEvaluation n (query2 x y n lastAnswer arr) arr queries -- Append number to string, update lastAnswer and continue to next query
-- Otherwise (queries are formatted incorrectly)
queryEvaluation _ _ _ _ = ""

dynamicArray :: Int -> [[Int]] -> [Char]
dynamicArray n queries = queryEvaluation n 0 (arrays n) queries

main :: IO ()
main = do
  (n:q:_) <- map read . words <$> getLine :: IO [Int]
  queries <- replicateM q $ do
    map read . words <$> getLine :: IO [Int]
  putStrLn $ dynamicArray n queries
