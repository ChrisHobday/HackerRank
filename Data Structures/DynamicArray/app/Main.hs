module Main where

-- import Data.Vector as V
import Data.Bits ( xor )
import Control.Monad ( replicateM )

arrays n = replicate n []

idx x n lastAnswer = (x `xor` lastAnswer) `mod` n

query1 x y n lastAnswer arr = arr1 ++ [ele ++ [y]] ++ arr2
  where (arr1, ele:arr2) = splitAt (idx x n lastAnswer) arr

query2 x y n lastAnswer arr = arr !! idx' !! (y `mod` length (arr !! idx'))
  where idx' = idx x n lastAnswer

dynamicArray _ [] = []
dynamicArray n ((q:x:y:_):queries) = queries


-- | null queries = ""
-- | q == 1       = "\nq1" ++ (dynamicArray n queries)
-- | otherwise    = "\nq2" ++ (dynamicArray n queries)

main :: IO ()
main = do
  (n:q:_) <- map read . words <$> getLine :: IO [Int]
  print n
  print q
  queries <- replicateM q $ do
    map read . words <$> getLine :: IO [Int]
  -- putStrLn $ dynamicArray n queries
  return ()
