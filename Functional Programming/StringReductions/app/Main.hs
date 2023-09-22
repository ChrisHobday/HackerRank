module Main (main) where

-- A given string with all duplicate characters removed
stringReduction :: [Char] -> [Char]
stringReduction string = stringReduction' "" string
  where
    -- Subfunction to encapsulate usedCharacters
    stringReduction' usedCharacters string'
      -- String is empty
      | null string'                    = reverse usedCharacters
      -- Character is already in used characters
      | character `elem` usedCharacters = stringReduction' usedCharacters restOfCharacters
      -- Otherwise (String is not empty and character iis not already used)
      | otherwise                       = stringReduction' (character : usedCharacters) restOfCharacters
      where
        (character : restOfCharacters) = string'

main :: IO ()
main = do
  string <- getLine -- Read and bind string
  putStrLn $ stringReduction string -- Print string reduction of entered string
