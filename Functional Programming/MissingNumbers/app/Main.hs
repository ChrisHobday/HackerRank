module Main (main) where

import qualified Data.Map as M

positiveDictionary :: (Ord k, Num a, Eq a) => M.Map k a -> k -> M.Map k a
positiveDictionary dictionary m = M.alter (\x -> if x == Nothing then Just 1 else (+ 1) <$> x) m dictionary

negativeDictionary :: (Ord k, Num a, Eq a) => M.Map k a -> k -> M.Map k a
negativeDictionary dictionary n = M.alter (\x -> if x == Nothing then Just (-1) else (+ (-1)) <$> x) n dictionary

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
