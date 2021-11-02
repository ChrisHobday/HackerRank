module Main where

-- A given list with the element at the given index removed
removeIndex :: Int -> [a] -> [a]
removeIndex index as = left ++ right
  where (left, _:right) = splitAt index as

-- If overcharge is 0 return "Bon Appetit" otherwise return the overcharge
checkOvercharge :: RealFrac a => a -> [Char]
checkOvercharge overcharge
  | overcharge == 0 = "Bon Appetit"
  | otherwise       = show $ round overcharge

-- The amount Brian overcharged Anna for the given bill, item not eaten and initial amount charged
bonAppetit :: Fractional a => [a] -> Int -> a -> a
bonAppetit bill itemNotEaten annaCharge = annaCharge - sum (removeIndex itemNotEaten bill) / 2

main :: IO ()
main = do
  nItemNotEatenTemp <- getLine -- Read line containing number of items in list to be given and item Anna didn't eat
  billTemp <- getLine -- Read line containing bill prices
  annaCharge <- readLn :: IO Double -- Read line containing amount Brian charged Anna
  let nItemNotEaten = map (read :: String -> Int) (words nItemNotEatenTemp) -- Convert read String to list of Ints
      itemNotEaten  = last nItemNotEaten -- Set item Anna didn't eat
      bill          = map (read :: String -> Double) (words billTemp) -- Convert read String to list of Doubles
  putStrLn $ checkOvercharge $ bonAppetit bill itemNotEaten annaCharge -- Print overcharge amount or "Bon Appetit" if overcharge is 0
