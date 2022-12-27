module Main (main) where

countAndRemove [] _ = 0
countAndRemove (firstChar:chars) char
  | firstChar == char = 1 + countAndRemove chars char

charCount :: [a] -> ()
charCount string = ()
  where firstChar = head string
        filteredString = filter (/= firstChar) string
        firstCharCount = count 

main :: IO ()
main = do
  string <- getLine
  print string
  return ()
