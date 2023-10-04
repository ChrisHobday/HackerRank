module Main (main) where

import Control.Monad ( replicateM )

-- Add a given integer to a given list of occurences or update the occurence counter for that given integer
-- Ex: addToIntegerList 5 [(1,1),(5,3)] = [(1,1),(5,4)]
addToIntegerList :: (Eq t, Num b) => t -> [(t, b)] -> [(t, b)]
addToIntegerList integer ((integer', numberOfOccurences) : integers)
  -- The given integer has been found in the given list of integers
  | integer == integer' = (integer', numberOfOccurences + 1) : integers
  -- Otherwise, the given integer has not found in the given list of integers (keep looking)
  | otherwise           = (integer', numberOfOccurences) : addToIntegerList integer integers
-- The given integer is not in the given list of integers
addToIntegerList integer [] = [(integer, 1)]

-- A list of the number of times a given list of integers each occurs
-- integerOccurencesList [1,1,2,5] [] = [(1,2),(2,1),(5,1)]
integerOccurencesList :: (Eq t, Num b) => [t] -> [(t, b)] -> [(t, b)]
integerOccurencesList (integer : restOfIntegers) integerList = integerOccurencesList restOfIntegers $ addToIntegerList integer integerList
integerOccurencesList [] integerList                         = integerList

-- A list of integers who have the required number of occurences from a given list of integers and their number of occurences
-- Ex: removeIntegers 2 [(1,3),(2,1)] = [1]
removeIntegers :: Ord t => t -> [(a, t)] -> [a]
removeIntegers repetitionCount ((integer, numberOfOccurences) : resotOfIntegerOccurences)
  | numberOfOccurences >= repetitionCount = integer : removeIntegers repetitionCount resotOfIntegerOccurences
  | otherwise                             = removeIntegers repetitionCount resotOfIntegerOccurences
removeIntegers _ [] = []

-- A list of integers from a given list of integers that occur at least the given amount of times
-- filterElements 2 [1,1,2,4,5,4,4] = [1,4]
filterElements :: (Ord a, Num a, Eq b) => a -> [b] -> [b]
filterElements repetitionCount integers = removeIntegers repetitionCount (integerOccurencesList integers [])

main :: IO ()
main = do
  testCases <- readLn :: IO Int -- Read and bind number of test cases to be entered
  testCaseIntegersThatOccurEnough <- replicateM testCases $ do -- Read and bind the test cases
    (_ : repetitionCount : _) <- (read <$>) . words <$> getLine :: IO [Int] -- Read and bind the repetition count
    integers <- (read <$>) . words <$> getLine :: IO [Int] -- Read and bind the list of integers to search for repetitions
    return $ filterElements repetitionCount integers -- Return the integers that occur enough
  mapM_ (\integersThatOccurEnough -> if not (null integersThatOccurEnough) then putStrLn (unwords (show <$> integersThatOccurEnough)) else putStrLn "-1") testCaseIntegersThatOccurEnough -- Print the integers that occur enough from each test case or "-1" if none occur enough, each on a seperate line
