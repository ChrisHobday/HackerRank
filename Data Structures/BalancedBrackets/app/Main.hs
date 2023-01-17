module Main (main) where

import Control.Monad ( replicateM )

-- Whether two given brackets match or not
-- Example: matchingBrackets '(' ']' = False
matchingBrackets :: Char -> Char -> Bool
matchingBrackets '(' ')' = True
matchingBrackets '{' '}' = True
matchingBrackets '[' ']' = True
matchingBrackets _ _     = False

-- Whether two given brackets allow a bracket string to continue
-- Example: continuingBrackets '(' '{' = True
continuingBrackets :: Char -> Char -> Bool
continuingBrackets '(' '(' = True
continuingBrackets '(' '[' = True
continuingBrackets '(' '{' = True
continuingBrackets '{' '(' = True
continuingBrackets '{' '[' = True
continuingBrackets '{' '{' = True
continuingBrackets '[' '(' = True
continuingBrackets '[' '[' = True
continuingBrackets '[' '{' = True
continuingBrackets _ _     = False

-- Whether a given bracket string is balanced or not
-- Example: isBalanced "{{[()]}}" = "YES"
isBalanced :: String -> String
isBalanced bracketString
  | go [] bracketString = "YES"
  | otherwise           = "NO"
  where -- Compares last unmatched bracket with next bracket
        go :: [Char] -> [Char] -> Bool
        -- There is both a unmatched bracket and a next bracket
        go (x : xs) (y : ys)
          -- Unmatched bracket and next bracket match
          | matchingBrackets x y   = go xs ys
          -- Unmatched bracket and next bracket allows for continuing
          | continuingBrackets x y = go (y : x : xs) ys
          -- Unmatched bracket and next bracket neither match or allow for continuing
          | otherwise              = False
        -- There is no unmatched bracket, but there is a next bracket
        go [] (y : ys) = go [y] ys
        -- There is a unmatched bracket, but no next bracket
        go (_ : _) []  = False
        -- There are no unmatched brackets and no next brackets
        go [] []       = True


main :: IO ()
main = do
  numberOfStrings <- (readLn :: IO Int) -- Read and bind number of strings to be entered
  bracketStrings <- replicateM numberOfStrings $ do -- Replicate the following action for each string to be entered
    getLine -- Read string

  mapM_ (putStrLn . isBalanced) bracketStrings -- Print wheter each bracket string is balanced or not