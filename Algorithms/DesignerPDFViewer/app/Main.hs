module Main where

import Data.Char ( ord )

-- The number representation of an alphabetic lowercase character
-- Converts the char to it's ascii number and than subtracts 97 (the ascii representation of 'a')
-- Ex. alphaNumber 'a' = 0, alphaNumber 'z' = 25
alphaNumber :: Char -> Int
alphaNumber c = ord c - 97

-- The max character height (mm) for a given list of characters and a word
-- Ex. maxHeight [1, 3, 1, 3, 1, 4, 1, 3, 2, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 7] "zaba" = 7
maxHeight :: [Int] -> String -> Int
-- There are no characters in given word
maxHeight h []      = 0
-- There are characters in given word
maxHeight h (c:cs)  = max (h !! alphaNumber c) (maxHeight h cs)

-- The area of rectangle highlight (mm squared) for a given list of character heights and a word assuming all character widths are 1mm
-- Ex. designerPdfViewer [1, 3, 1, 3, 1, 4, 1, 3, 2, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 7] "zaba" = 28
designerPdfViewer :: [Int] -> String -> Int
designerPdfViewer h word = maxHeight h word * length word

main :: IO ()
main = do
  h <- map read . words <$> getLine :: IO [Int] -- Read list of character heights in mm
  word <- getLine -- Read word to find area of rectangle highlight
  print $ designerPdfViewer h word -- Print the area of the rectangle highlight for given character heights and word