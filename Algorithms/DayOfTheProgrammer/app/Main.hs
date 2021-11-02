module Main where

-- If a given year is a julian year
isJulianYear :: Int -> Bool
isJulianYear year
  | year < 1918 = True
  | otherwise   = False

-- If a given year is a gregorian year
isGregorianYear :: Int -> Bool
isGregorianYear year
  | year > 1918 = True
  | otherwise   = False

-- If a given julian year is a leap year
isJulianLeapYear :: Int -> Bool
isJulianLeapYear year
  | year `mod` 4 == 0 = True
  | otherwise         = False

-- If a given gregorian year is a leap year
isGregorianLeapYear :: Int -> Bool
isGregorianLeapYear year
  | year `mod` 400 == 0 = True
  | year `mod` 100 == 0 = False
  | year `mod` 4 == 0   = True
  | otherwise           = False

-- The day of the programmer for a given year
dayOfProgrammer :: Int -> String
dayOfProgrammer year
  | isJulianYear year =
    case isJulianLeapYear year of
      True -> "12.09." ++ show year
      False -> "13.09." ++ show year
  | isGregorianYear year =
    case isGregorianLeapYear year of
      True -> "12.09." ++ show year
      False -> "13.09." ++ show year
  | otherwise = "26.09." ++ show year
      

main :: IO ()
main = do
  year <- readLn :: IO Int -- Read Line containing year
  putStrLn $ dayOfProgrammer year -- Print the day of the programmer