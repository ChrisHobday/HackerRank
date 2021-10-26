module Main where

-- Calculate the number of apples and oranges that fall on Sam's house with given house location, tree locations, and apples and orange distances
countApplesAndOranges :: Int -> Int -> Int -> Int -> [Int] -> [Int] -> (Int,Int)
countApplesAndOranges s t a b as os = (length (filter (>=s) (filter (<=t) (fmap (+a) as))),length(filter (>=s) (filter (<=t) (fmap (+b) os))))

main :: IO ()
main = do
  houseTemp <- getLine -- Read line containing house start and end
  appleOrangeLocationTemp <- getLine -- Read line containing apple and orange locations
  _ <- getLine -- Read line containing apple and orange sizes but don't bind it cause we don't need it
  applesTemp <- getLine -- Read line containing apple distances
  orangesTemp <- getLine -- Read line containing orange distances
  let st      = map (read :: String -> Int) (words houseTemp) -- Convert read line to house start and end
      s       = head st -- Set house start
      t       = last st -- Set house end
      ab      = map (read :: String -> Int) (words appleOrangeLocationTemp) -- Convert read line  to apple orange locations
      a       = head ab -- Set apple location
      b       = last ab -- Set orange location
      as      = map (read :: String -> Int) (words applesTemp) -- Convert read line to apple distances
      os      = map (read :: String -> Int) (words orangesTemp) -- Convert read line to orange distances
      (al,ol) = countApplesAndOranges s t a b as os -- Calculate and set the number of apples and oranges that land on Sam's house
  print al
  print ol
