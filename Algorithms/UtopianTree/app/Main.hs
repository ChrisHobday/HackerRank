module Main (main) where

utopianTree :: Int -> Int
utopianTree n = undefined

main :: IO ()
main = do
  growthCycles <- readLn :: IO Int -- Read number of growth cycles
  print $ utopianTree growthCycles