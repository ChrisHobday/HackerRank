module Main (main) where

stringReduction string = stringReduction' "" string
  where
    stringReduction' usedCharacters string'
      | null string'                    = reverse usedCharacters
      | character `elem` usedCharacters = stringReduction' usedCharacters restOfCharacters
      | otherwise                       = stringReduction' (character : usedCharacters) restOfCharacters
      where
        (character : restOfCharacters) = string'

main :: IO ()
main = do
  string <- getLine -- Read and bind string
  putStrLn $ stringReduction string -- Print string reduction of entered string
