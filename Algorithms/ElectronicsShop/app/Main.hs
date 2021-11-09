module Main where

-- The maximum one can spend on one given keyboard from list of given keyboard prices and one given usb drive from list of given usb drives within the given budget
getMoneySpent :: (Ord a, Num a) => [a] -> [a] -> a -> a
getMoneySpent keyboards usbs budget = maximum $ (-1) : filter (<= budget) [x + y | x <- keyboards, y <- usbs]

main :: IO ()
main = do
  (budget:_) <- map read . words <$> getLine :: IO [Int] -- Read budget, number of keyboards, and number of usbs and bind budget
  keyboards <- map read . words <$> getLine :: IO [Int] -- Read list of keyboard prices and bind it
  usbs <- map read . words <$> getLine :: IO [Int] -- Read list of usb prices and bind it
  print $ getMoneySpent keyboards usbs budget -- Print the maximum one can spend on one keyboard and one usb drive within the given budget
