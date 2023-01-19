module Main (main) where

encrypted string = undefined

main :: IO ()
main = do
  string <- getLine -- Read and bind string
  putStrLn $ encrypted string
