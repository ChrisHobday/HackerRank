module Main (main) where

import Control.Monad ( replicateM
                     , mapM_ )

isBalanced bracketString = bracketString

main :: IO ()
main = do
  numberOfStrings <- (readLn :: IO Int) -- Read and bind number of strings to be entered
  bracketStrings <- replicateM numberOfStrings $ do -- Replicate the following action for each string to be entered
    getLine -- Read string

  mapM_ (print . isBalanced) bracketStrings -- Print wheter each bracket string is balanced or not