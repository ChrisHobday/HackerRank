module Main where

helloWorldNTimes :: Int -> IO ()
helloWorldNTimes n
  | n /= 0    = do
    putStrLn "Hello World"
    helloWorldNTimes (n - 1)
  | otherwise = return ()

main :: IO ()
main = do
  n <- readLn :: IO Int
  helloWorldNTimes n
