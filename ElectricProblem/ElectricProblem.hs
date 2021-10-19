module Main where

import Control.Applicative
  ( liftA2 )

main :: IO ()
main = do
  print $ getMoneySpent [50,50,0,100,95] [5,15,30] 100
  return ()

getMoneySpent :: [Int] -> [Int] -> Int -> Int
getMoneySpent keyboards drives budget
  | null possibleMoneySpent = -1
  | otherwise                = maximum possibleMoneySpent
  where possibleMoneySpent = filter (<= budget) (liftA2 (+) keyboards drives)
