module Main (main) where

-- -- A given list filtered to remove every other element starting with the first
f :: [Int] -> [Int]
f (oddElement : evenElement : otherElements) = evenElement : f otherElements
f [oddElement]                               = []
f []                                         = []

-- This part deals with the Input and Output and can be used as it is. Do not modify it.
main = do
  inputdata <- getContents -- Read and bind list of numbers as string

  mapM_ (putStrLn. show). f. map read. lines $ inputdata -- Convert entered string to list of numbers and print the result of filtering every other element
