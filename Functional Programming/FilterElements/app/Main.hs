module Main (main) where

import qualified Data.Map as Map
import Control.Monad ( replicateM )

-- The number of times each integer occurs in a given list (as a key-value map)
-- Ex: integerOccurencesMap [1,2,3,1] Map.empty = fromList [(1,2),(2,1),(3,1)]
-- integerOccurencesMap :: (Foldable t, Ord k, Num a) => t k -> Map.HashMap k a -> Map.HashMap k a
integerOccurencesMap :: (Foldable t, Ord k, Num a) => t k -> Map.Map k a -> Map.Map k a
integerOccurencesMap integers map = foldl (\map' integer' -> Map.insertWith (+) integer' 1 map') map integers

-- A list of integers the occur the given amount of times
-- Ex: filterElements 2 [1,1,2,3,2] = [1,2]
filterElements :: (Foldable t, Ord a, Ord k, Num a) => a -> t k -> [k]
filterElements repetitionCount integers = Map.keys $ Map.filter (>= repetitionCount) $ integerOccurencesMap integers Map.empty

main :: IO ()
main = do
  testCases <- readLn :: IO Int -- Read and bind number of test cases to be entered
  testCaseIntegersThatOccurEnough <- replicateM testCases $ do -- Read and bind the test cases
    (_ : repetitionCount : _) <- (read <$>) . words <$> getLine :: IO [Int] -- Read and bind the repetition count
    integers <- (read <$>) . words <$> getLine :: IO [Int] -- Read and bind the list of integers to search for repetitions
    return $ filterElements repetitionCount integers -- Return the integers that occur enough
  mapM_ (\integersThatOccurEnough -> if not (null integersThatOccurEnough) then putStrLn (unwords (show <$> integersThatOccurEnough)) else putStrLn "-1") testCaseIntegersThatOccurEnough -- Print the integers that occur enough from each test case or "-1" if none occur enough, each on a seperate line