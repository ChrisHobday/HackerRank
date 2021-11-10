module Main where

import System.IO( hSetBuffering, stdout, BufferMode( BlockBuffering ) )
import Control.Monad ( forM_ )

-- Which cat that gets the mouse or if the mouse gets away
catAndMouse :: (Num a, Ord a) => a -> a -> a -> [Char]
catAndMouse x y z
  | abs (x - z) == abs (y - z) = "Mouse C"
  | abs (x - z) < abs (y - z)  = "Cat A"
  | otherwise                  = "Cat B"

-- Read a given number of lines
-- getLines :: Int -> IO [String]
-- getLines 0 = return []
-- getLines n = do
--     line <- getLine
--     rest <- getLines (n - 1)
--     return (line : rest)

main :: IO ()
main = do
  -- stdout <- getEnv "OUTPUT_PATH" -- Bind stdout
  -- fptr <- openFile stdout WriteMode -- Bind handle to stdout

  hSetBuffering stdout (BlockBuffering Nothing) -- Set stdout buffering to BlockBuffering

  nQueries <- readLn :: IO Int -- Read number of queries to be entered
  forM_ [1..nQueries] $ \q_itr -> do -- Loop the given number of queries
    (x:y:z:_) <- map read . words <$> getLine :: IO [Int] -- Read cat a, cat b, and mouse c position and bind them
    putStrLn $ catAndMouse x y z -- Print the cat that gets the moues or if the mouse gets away
  
  -- hFlush stdout -- Flush handle to stdout
  -- hClose stdout -- Close handle to stdout
