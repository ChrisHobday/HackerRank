module Main (main) where

-- A prefix compression of 2 given strings
prefixCompression :: [Char] -> [Char] -> [Char]
prefixCompression string string2 = prefixCompression' 0 "" string string2
  where
    -- Subfunction to encapsulate numberOfSameCharacters and sameCharacters
    prefixCompression' numberOfSameCharacters sameCharacters string' string2'
      -- First or second string is empty
      | null string' || null string2' || firstCharacter /= firstCharacter2 =
        -- Check length of the same characters
        case numberOfSameCharacters of
          -- There was not previously matching charactes
          0 -> show (length string') ++ " " ++ string' ++ "\n" ++ show (length string2') ++ " " ++ string2'
          -- There was previously matchiing characters
          _ -> show numberOfSameCharacters ++ " " ++ sameCharacters ++ "\n" ++ show (length string') ++ " " ++ string' ++ "\n" ++ show (length string2') ++ " " ++ string2'
      -- Otherwise (First character of both strings match)
      | otherwise = prefixCompression' (numberOfSameCharacters + 1) (sameCharacters ++ [firstCharacter]) restofString restofString2
      where
        (firstCharacter : restofString)    = string'
        (firstCharacter2 : restofString2)  = string2'

main :: IO ()
main = do
  string <- getLine -- Read and bind first string
  string2 <- getLine -- Read and bind second string

  putStr $ prefixCompression string string2 -- Print the prefix compression of the 2 entered strings