module Main (main) where

-- Note: this exploits the fact that minutes from 1-12 have the same word
-- A time number (hour/minute) to a word
timeNumberToWord :: Int -> String
timeNumberToWord number
  -- Number is outside of range 0-59
  | number < 0 || number > 59 = "unknown"
  | number == 0               = "o' clock"
  | number == 1               = "one"
  | number == 2               = "two"
  | number == 3               = "three"
  | number == 4               = "four"
  | number == 5               = "five"
  | number == 6               = "six"
  | number == 7               = "seven"
  | number == 8               = "eight"
  | number == 9               = "nine"
  | number == 10              = "ten"
  | number == 11              = "eleven"
  | number == 12              = "twelve"
  | number == 13              = "thirteen"
  | number == 14              = "fourteen"
  | number == 15              = "quarter"
  | number == 16              = "sixteen"
  | number == 17              = "seventeen"
  | number == 18              = "eighteen"
  | number == 19              = "nineteen"
  | number == 20              = "twenty"
  | number == 21              = "twenty one"
  | number == 22              = "twenty two"
  | number == 23              = "twenty three"
  | number == 24              = "twenty four"
  | number == 25              = "twenty five"
  | number == 26              = "twenty six"
  | number == 27              = "twenty seven"
  | number == 28              = "twenty eight"
  | number == 29              = "twenty nine"
  | number == 30              = "half"
  -- Otherwise (number is in range 31-59)
  | otherwise                 = timeNumberToWord (60 - number) -- Subtract number from 60 and run it through again

-- The time from hour (int) and minute (int) to words
timeInWords :: Int -> Int -> String
timeInWords hour minute
  -- Minute is outside of range 0-59
  | minute < 0 || minute > 59    = "unknown"
  | minute == 0                  = timeNumberToWord hour ++ " " ++ timeNumberToWord minute
  | minute == 1                  = timeNumberToWord minute ++ " minute past " ++ timeNumberToWord hour
  | minute == 15 || minute == 30 = timeNumberToWord minute ++ " past " ++ timeNumberToWord hour
  | minute < 30                  = timeNumberToWord minute ++ " minutes past " ++ timeNumberToWord hour
  | minute == 45                 = timeNumberToWord minute ++ " to " ++ timeNumberToWord (hour + 1)
  | minute == 59                 = timeNumberToWord minute ++ " minute to " ++ timeNumberToWord (hour + 1)
  | otherwise                    = timeNumberToWord minute ++ " minutes to " ++ timeNumberToWord (hour + 1)

main :: IO ()
main = do
  hour <- readLn :: IO Int -- Read hour and bind it
  minute <- readLn :: IO Int -- Read minute and bind it
  putStrLn $ timeInWords hour minute -- Print given time in words