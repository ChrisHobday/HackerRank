module Main (main) where

import qualified Data.Map as Map
import Data.Maybe ( isJust, fromJust )
import Data.List ( nub )
import Control.Monad ( replicateM )

-- Note: This solution uses key-value maps for tallying up the number of occurences for each integer, but also has to compare the map with a nubbed list of the integers because of a quirk with maps
-- where they automatically sort the keys and as a result produce the wrong order when trying to output them.

-- The number of times each integer occurs in a given list (as a key-value map)
-- Ex: integerOccurencesMap [1,2,3,1] Map.empty = fromList [(1,2),(2,1),(3,1)]
integerOccurencesMap :: (Foldable t, Ord k, Num a) => t k -> Map.Map k a -> Map.Map k a
integerOccurencesMap integers integerMap = foldl (\integerMap' integer' -> Map.insertWith (+) integer' 1 integerMap') integerMap integers

-- A list of integers from a given list that occur at least the given amount of times lookuped up from a given map
-- Ex: integersThatOccurEnough 2 [1,2,3] (Map.fromList[(1,2),(2,0),(3,5)]) = [1,3]
integersThatOccurEnough :: (Ord t, Ord a) => t -> [a] -> Map.Map a t -> [a]
-- There is at least 1 integer in the given list
integersThatOccurEnough repetitionCount (integer : restOfIntegers) integerMap
  -- The value of the lookedup integer exists and is greater than or equal to the given repetition count
  | isJust maybeIntegerNumberOfOccurences && integerNumberOfOccurences >= repetitionCount = integer : integersThatOccurEnough repetitionCount restOfIntegers integerMap
  -- Otherwise, the value of the lookedup integer does not exists or is not greater than or equal to the given repetition count
  | otherwise                                                                             = integersThatOccurEnough repetitionCount restOfIntegers integerMap
  where
    -- Maybe the value of the lookedup integer key
    maybeIntegerNumberOfOccurences = Map.lookup integer integerMap
    -- The value of the lookedup integer key
    integerNumberOfOccurences      = fromJust maybeIntegerNumberOfOccurences
-- There are no integers to check the occurences of
integersThatOccurEnough _ [] _ = []

-- A list of integers the occur the given amount of times
-- Ex: filterElements 2 [1,1,2,3,2] = [1,2]
filterElements :: (Ord t, Ord a, Num t) => t -> [a] -> [a]
filterElements repetitionCount integers = integersThatOccurEnough repetitionCount (nub integers) (integerOccurencesMap integers Map.empty)

main :: IO ()
main = do
  testCases <- readLn :: IO Int -- Read and bind number of test cases to be entered
  testCaseIntegersThatOccurEnough <- replicateM testCases $ do -- Read and bind the test cases
    (_ : repetitionCount : _) <- (read <$>) . words <$> getLine :: IO [Int] -- Read and bind the repetition count
    integers <- (read <$>) . words <$> getLine :: IO [Int] -- Read and bind the list of integers to search for repetitions
    return $ filterElements repetitionCount integers -- Return the integers that occur enough
  mapM_ (\integersThatOccurEnough -> if not (null integersThatOccurEnough) then putStrLn (unwords (show <$> integersThatOccurEnough)) else putStrLn "-1") testCaseIntegersThatOccurEnough -- Print the integers that occur enough from each test case or "-1" if none occur enough, each on a seperate line