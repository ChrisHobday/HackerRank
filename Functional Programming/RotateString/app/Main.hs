module Main (main) where

import Control.Monad ( replicateM
                     , mapM_ )

rotate (char : chars) = chars <> [char]

rotateN chars = take n $ drop 1 $ iterate rotate chars
  where n = length chars

main :: IO ()
main = do
  nStrings <- readLn :: IO Int

  stringRotations <- replicateM nStrings $ do
    string <- getLine
    return $ rotateN string
  
  mapM_ (putStrLn . unwords) stringRotations