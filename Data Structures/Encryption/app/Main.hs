module Main (main) where

import Data.List ( genericLength
                 , transpose
                 , intercalate )
import Data.Char ( isSpace )

-- A given string split up into a given number of chunks
-- Example: splitUp 3 "chillout" = ["chi","llo","ut"]
splitUp :: Int -> [a] -> [[a]]
splitUp _ [] = []
splitUp n string  = firstSegment : splitUp n restOfSegments
  where (firstSegment, restOfSegments) = splitAt n string

-- The encrypted version of a given string
-- Example: encrypted "chillout" = "clu hlt io"
encrypted :: String -> String
encrypted string = unwords $ transpose $ splitUp chunkSize stringNoSpace
  where -- The given string with it's spaces removed
        stringNoSpace = filter (not . isSpace) string
        -- The size of chunks to split the string into
        chunkSize = ceiling $ sqrt $ genericLength stringNoSpace
        
main :: IO ()
main = do
  string <- getLine -- Read and bind string
  putStrLn $ encrypted string -- Print the string encrypted
