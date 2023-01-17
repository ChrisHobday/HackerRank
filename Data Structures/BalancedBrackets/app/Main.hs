module Main (main) where

import Control.Monad ( replicateM
                     , mapM_ )

-- Whether two given brackets match or not
-- Example: matchingBrackets '(' ']' = False
matchingBrackets :: Char -> Char -> Bool
matchingBrackets '(' ')' = True
matchingBrackets '{' '}' = True
matchingBrackets '[' ']' = True
matchingBrackets _ _     = False

isBalanced :: [a] -> ([a], [a])
isBalanced bracketString = (firstHalf, secondHalf)
  where (firstHalf, secondHalf) = splitAt (length bracketString `div` 2) bracketString
        go bracket bracket'
          | bracket == bracket' = True
          | otherwise           = False

main :: IO ()
main = do
  numberOfStrings <- (readLn :: IO Int) -- Read and bind number of strings to be entered
  bracketStrings <- replicateM numberOfStrings $ do -- Replicate the following action for each string to be entered
    getLine -- Read string

  mapM_ (print . isBalanced) bracketStrings -- Print wheter each bracket string is balanced or not