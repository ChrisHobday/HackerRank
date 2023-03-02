module Main (main) where

import Control.Monad
  ( replicateM 
  , mapM_ )

evaluate number = undefined

main :: IO ()
main = do
  numberOfNumberToBeEntered <- readLn :: IO Int -- Read and bind number of numbers to be entered

  numbers <- replicateM numberOfNumberToBeEntered $ do -- For each number to be entered...
    readLn :: IO Float -- Read number
  
  mapM_ evaluate numbers
