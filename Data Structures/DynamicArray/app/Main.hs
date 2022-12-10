module Main where

-- import Data.Vector as V
import Data.Bits ( xor )

array :: [[Int]]
array = [[],[]]

-- array :: Vector Vector Int
-- array = V.replicate 5 (V.replicate 5 0)

lastAnswer :: Int
lastAnswer = 0

idx :: Int -> Int -> Int
idx x n = (x `xor` lastAnswer) `mod` n

Query1 x y n arr = arr1:y:arr2
  where (arr1, arr2) = splitAt (idx x n)

-- idx :: Int -> Int -> Int -> Int
-- idx queryValue1 lastAnswer arraySize = (queryValue1 `xor` lastAnswer) `mod` arraySize

-- onceThrough :: Vector Int -> Int -> Int -> Int -> Int -> Vector Int
-- onceThrough array queryValue1 lastAnswer arraySize queryValue2 = array // [(idx queryValue1 lastAnswer arraySize,queryValue2)]

-- onceThrough :: [[Int]] -> Int -> Int -> Int -> Int -> [[Int]]
-- onceThrough array queryValue1 lastAnswer arraySize queryValue2 = 
--   arraySplit1 ++ (yArray ++ [queryValue2]) : arraySplit2
--   where (arraySplit1,yArray:arraySplit2) = splitAt (idx queryValue1 lastAnswer arraySize) array

-- onceThrough2 :: [[Int]] -> Int -> Int -> Int -> Int -> Int
-- onceThrough2 array queryValue1 lastAnswer arraySize queryValue2 = 
--   head $ array !! idx queryValue1 lastAnswer arraySize

main :: IO ()
main = print ""
