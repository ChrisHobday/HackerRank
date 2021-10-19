{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where

import Control.Monad
import Data.Array
import Data.Bits
import Data.List
import Data.List.Split
import Data.Set
import Data.Text
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe

-- Complete the compareTriplets function below.
compareTriplets :: [Int] -> [Int] -> [Int]
compareTriplets [] _ = [0, 0]
compareTriplets _ [] = [0, 0]
compareTriplets (a:as) (b:bs)
    | a > b  = Data.List.zipWith (+) [1, 0] (compareTriplets as bs)
    | b > a  = Data.List.zipWith (+) [0, 1] (compareTriplets as bs)
    | a == b = Data.List.zipWith (+) [0, 0] (compareTriplets as bs)

lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack
rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

main :: IO()
main = do
    aTemp <- getLine
    let a = Data.List.map (read :: String -> Int) . Data.List.words $ rstrip aTemp
    bTemp <- getLine
    let b = Data.List.map (read :: String -> Int) . Data.List.words $ rstrip bTemp
    let result = compareTriplets a b
    print result