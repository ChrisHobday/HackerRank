module Main (main) where

import Text.Printf (printf)

solve :: Double -> Double -> [Double] -> [Double] -> [Double]
solve lowerRangeLimit upperRangeLimit coefficients exponents = [area, volume]
  where
    subintervalLength = 0.001
    domain = [lowerRangeLimit, lowerRangeLimit + subintervalLength .. upperRangeLimit]
    bigF d = sum $ zipWith (\coefficient exponent -> coefficient*d**exponent) coefficients exponents
    area   = sum $ map ((* subintervalLength) . bigF) domain
    volume = sum $ map (((* subintervalLength) . (\r -> pi * r ** 2)) . bigF) domain

--Input/Output.
main :: IO ()
main = getContents >>= mapM_ (printf "%.1f\n"). (\[a, b, [l, r]] -> solve l r a b). map (map read. words). lines
