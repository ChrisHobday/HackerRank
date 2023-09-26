module Main (main) where

-- A given string compressed
-- Ex : stringCompression 1 "abcaaabbb" = "abca3b3"
stringCompression :: (Num a, Eq a, Show a) => a -> String -> String
-- There are at least 2 characters in given string
stringCompression numCharsSame (char1 : char2 : restOfChars)
  -- The first 2 characters are the same
  | char1 == char2    = stringCompression (numCharsSame + 1) (char2 : restOfChars)
  -- The number of previously same characters is 1
  | numCharsSame == 1 = char1 : (stringCompression 1 $ char2 : restOfChars)
  -- Otherwise (The first 2 characters are different and the number of previously same characters is not 1)
  | otherwise         = char1 : (show numCharsSame) ++ (stringCompression 1 $ char2 : restOfChars)
-- There are less than 2 characters in given string
stringCompression numCharsSame string
  -- The number of previously same characters is 1
  | numCharsSame == 1 = string
  -- Otherwise (The number of previously same characters is not 1)
  | otherwise         = string ++ (show numCharsSame)

main :: IO ()
main = do
  string <- getLine -- Read and bind string to compress
  putStrLn $ stringCompression 1 string -- Print the given string compressed
