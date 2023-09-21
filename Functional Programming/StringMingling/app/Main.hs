module Main (main) where

-- A string wiith mingled characters of two given strings (starting with the first string)
mingled :: [Char] -> [Char] -> [Char]
mingled [] _ = ""
mingled _ [] = ""
mingled (character : restOfString) (character2 : restOfString2) = character : character2 : mingled restOfString restOfString2

main :: IO ()
main = do
  string <- getLine -- Read and bind the first string
  string2 <- getLine -- Read and bind the second string
  putStrLn $ mingled string string2 -- Print the two input strings mingled together