module Main (main) where

import qualified Data.Map as M

-- A given dictionary/map with either 1 added to the given key or if the given key does not exist, create it and set it's value to 1
-- Example: positiveDictionary M.empty 3 = fromList [(3, 1)]
-- Example: positiveDictionary (M.fromList [(3, 50)]) 3 = fromList [(3,51)]
positiveDictionary :: (Ord k, Num a, Eq a) => M.Map k a -> k -> M.Map k a
positiveDictionary dictionary m = M.alter (\x -> if x == Nothing then Just 1 else (+ 1) <$> x) m dictionary

-- A given dictionary/map with either 1 subtracted to the given key or if the given key does not exist, create it and set it's value to -1
-- Example: negativeDictionary M.empty 3 = fromList [(3,-1)]
-- Example: negativeDictionary (M.fromList [(3, 50)]) 3 = fromList [(3,49)]
negativeDictionary :: (Ord k, Num a, Eq a) => M.Map k a -> k -> M.Map k a
negativeDictionary dictionary n = M.alter (\x -> if x == Nothing then Just (-1) else (+ (-1)) <$> x) n dictionary

-- A dictionary with a running tally of the occurences of a given list of numbers minus another given list of numbers
-- Example: missingNumbers M.empty [203,204,205] [203,204,205,203] = fromList [(203,1),(204,0),(205,0)]
missingNumbers :: (Ord a1, Num a2, Eq a2) => M.Map a1 a2 -> [a1] -> [a1] -> M.Map a1 a2
missingNumbers dictionary (n : ns) (m : ms) = missingNumbers newDictionary ns ms
  where
    newDictionary = positiveDictionary (negativeDictionary dictionary n) m
missingNumbers dictionary [] (m : ms) = missingNumbers (positiveDictionary dictionary m) [] ms
missingNumbers dictionary (n : ns) [] = missingNumbers (negativeDictionary dictionary n) ns []
missingNumbers dictionary [] []       = dictionary

main :: IO ()
main = do
  _ <- readLn :: IO Int -- Ignore user entered integer
  ns <- (read <$>) . words <$> getLine :: IO [Int] -- Read and bind the first list of user entered integers
  _ <- readLn :: IO Int -- Ignore user entered integer
  ms <- (read <$>) . words <$> getLine :: IO [Int] -- Read and bind the second list of user entered integers

  putStrLn $ unwords $ show <$> M.keys (M.filter (> 0) $ missingNumbers M.empty ns ms) -- Print each unqiue integer in the second list but not the first, sorted
