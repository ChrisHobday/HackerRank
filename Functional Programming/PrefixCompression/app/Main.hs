module Main (main) where
  
prefixCompression sameCharacters string string2
  -- First or second string is empty
  | null string || null string2 || firstCharacter /= firstCharacter2 =
    -- Check length of the same characters
    case length sameCharacters of
      -- There was not previously matching charactes
      0 -> show (length string) ++ " " ++ string ++ "\n" ++ show (length string2) ++ " " ++ string2
      -- There was previously matchiing characters
      _ -> show (length sameCharacters) ++ " " ++ sameCharacters ++ "\n" ++ show (length string) ++ " " ++ string ++ "\n" ++ show (length string2) ++ " " ++ string2
  -- Otherwise (First character of both strings match)
  | otherwise = prefixCompression (sameCharacters ++ [firstCharacter]) restofString restofString2
  where
    (firstCharacter : restofString)    = string
    (firstCharacter2 : restofString2)  = string2

main :: IO ()
main = undefined
