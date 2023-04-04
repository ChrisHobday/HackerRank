module Main (main) where

gcd' x y = undefined

main :: IO ()
main = do
  (x : _ : y : _) <- (read <$>) . words <$> getLine :: IO [Int] -- Read two number to find GCD between and bind them to x and y
  print $ gcd' x y
