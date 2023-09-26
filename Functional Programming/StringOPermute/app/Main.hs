module Main (main) where

import Control.Monad ( replicateM )

-- A given string with it's even characters swapped with the previous character
-- Ex: swapEvenCharacters "abcde" = "badce"
-- swapEvenCharacters :: [a] -> [a]
-- swapEvenCharacters string
--   -- Length of given string is less than 2 (not enough characters to swap)
--   | length string < 2 = string
--   -- Otherwise (length of given string is not less than 2)
--   | otherwise         = char2 : char1 : swapEvenCharacters restOfChars
--   where
--     (char1 : char2 : restOfChars) = string

-- A given string with it's even characters swapped with the previous character
-- Ex: swapEvenCharacters "abcde" = "badce"
swapEvenCharacters :: [a] -> [a]
-- There are enough characters to swap (2 or more)
swapEvenCharacters (char1 : char2 : restOfChars) = char2 : char1 : swapEvenCharacters restOfChars
-- Otherwise (There aren't enough character to swap (less than 2))
swapEvenCharacters string                        = string

main :: IO ()
main = do
  numberOfTestCases <- readLn :: IO Int -- Read number of test cases to be entered and bind it
  testCases <- replicateM numberOfTestCases $ do -- Replicate the following action for each test case
    string <- getLine -- Read and bind the string to swap even characters on
    return $ swapEvenCharacters string -- Return user entered string with characters swapped
  mapM_ putStrLn testCases -- Print the list of given string with their even characters swapped (one per line)
  