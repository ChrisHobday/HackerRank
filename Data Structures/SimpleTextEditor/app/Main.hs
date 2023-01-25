module Main (main) where

import Control.Monad ( replicateM )
import Data.Maybe ( fromJust )

-- Evaluate a given operation with a given list of strings, returning the new list of strings
-- Example: ["abc",""] <- evaluateOperation (1, Just "abc") [""]
evaluateOperation :: (Eq a, Num a) => (a, Maybe String) -> [String] -> IO [String]
evaluateOperation (operationType, argument) (string : strings)
  -- Operation is append characters
  | operationType == 1 = do
                           return (appendedString : string : strings) -- Return list of strings with new appended string in front
  -- Operation is delete an amount of characters
  | operationType == 2 = do
                           return (deletedString : string : strings) -- Return list of strings with new deleted string in front
  -- Operation is print a character
  | operationType == 3 = do
                           putStrLn [(string !! characterToPrint)] -- Print proper character
                           return (string : strings) -- Return list of strings
  -- Operation is undo
  | operationType == 4 = do
                           return strings -- Return list of strings with previous string removed
  where -- The previous string with characters appended to the end
        appendedString = string ++ fromJust argument
        -- The number of characters to delete from the end of previous string (only evaluated if operation type is 2)
        numberOfCharactersToDelete = read $ fromJust argument :: Int
        -- The previous string with characters deleted from the end
        deletedString = take (length string - numberOfCharactersToDelete) string
        -- Character to print
        characterToPrint = (read $ fromJust argument) - 1 :: Int

-- Evaluate a list of operations on a list of strings
-- Example: evaluateOperations [(1, Just "abc"), (3, Just "2")] [""] = prints b to the console
evaluateOperations :: (Eq a, Num a) => [(a, Maybe String)] -> [String] -> IO ()
evaluateOperations [] _ = do return ()
evaluateOperations (operation : restOfoperations) strings = do
                                                newStrings <- evaluateOperation operation strings -- Evaluate operation, binding resulting new list of strings
                                                evaluateOperations restOfoperations newStrings -- Evaluate rest of operations using new list of strings

main :: IO ()
main = do
  numberOfOperations <- (readLn :: IO Int) -- Read and bind number of operations to be entered
  operations <- replicateM numberOfOperations $ do -- For each operation to be entered...
    (operationType : argument) <- words <$> getLine -- Read and bind operation type and potential argument
    let operationType' = read operationType :: Int -- Operation type as an integer
    let argument'      = head argument -- Potential argument (not evaluated unless later needed)
    case operationType' of
      -- Operation is 4 (undo) and therefore has no argument
      4 -> return (operationType', Nothing)
      -- Operation has an argument
      _ -> return (operationType', Just argument')
  
  evaluateOperations operations [""] -- Evaluate the entered operations with an empty starting string
  
