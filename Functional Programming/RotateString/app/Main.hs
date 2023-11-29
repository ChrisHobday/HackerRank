module Main (main) where

import Control.Monad ( replicateM
                     , mapM_ )

-- A given string with it's first character moved to the back
-- Example: rotate "abc" = "bca"
rotate (char : chars) = chars <> [char]

-- A list of all the rotations of a given string
-- Example: rotateN "abc" = ["bca","cab","abc"]
rotateN chars = take n $ drop 1 $ iterate rotate chars
  where n = length chars

main :: IO ()
main = do
  nStrings <- readLn :: IO Int -- Read and bind the number of string to be entered

  stringRotations <- replicateM nStrings $ do -- Preform the following monadic function nString times and bind the results to the list of string rotations
    string <- getLine -- Read and bind a string
    return $ rotateN string -- Return the list of all the rotations on the given string
  
  mapM_ (putStrLn . unwords) stringRotations -- Print all the lists of all the rotations on the given strings