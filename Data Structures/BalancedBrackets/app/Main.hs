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
-- isBalanced :: String -> String
isBalanced bracketString
  | go [] bracketString = "YES"
  | otherwise           = "NO"
  where -- Compares latest unmatched bracket with next bracket in bracket string
        go :: [Char] -> [Char] -> Bool
        go (x : xs) (y : ys)
          | matchingBrackets x y   = go xs ys
          | continuingBrackets x y = go (y : x : xs) ys
          | otherwise              = False
        go [] (y : ys) = go [y] ys
        go (x : xs) [] = False
        go [] []       = True


main :: IO ()
main = do
  numberOfStrings <- (readLn :: IO Int) -- Read and bind number of strings to be entered
  bracketStrings <- replicateM numberOfStrings $ do -- Replicate the following action for each string to be entered
    getLine -- Read string

  mapM_ (putStrLn . isBalanced) bracketStrings -- Print wheter each bracket string is balanced or not