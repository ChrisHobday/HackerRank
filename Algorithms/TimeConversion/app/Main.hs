module Main where

-- Check if given time is AM
checkAM :: String -> Bool
checkAM t
  | t !! 8 == 'A' = True
  | otherwise     = False

-- Check if given time is PM
checkPM :: String -> Bool
checkPM t
  | t !! 8 == 'P' = True
  | otherwise     = False

check12 :: String -> Bool
check12 t
  | t !! 0 == '1' && t !! 1 == '2' = True
  | otherwise                      = False

timeConversion :: String -> String
timeConversion t
  | checkAM t && check12 t = "00" ++ drop 2 (init $ init t)
  | checkAM t              = init $ init t
  | checkPM t && check12 t = init $ init t
  | checkPM t              = show ((read [t !! 0, t !! 1] :: Int) + 12) ++ drop 2 (init $ init t)
  | otherwise              = "??:??:??"

main :: IO ()
main = do
  time12 <- getLine -- Read 12 time
  putStrLn $ timeConversion time12 -- Print converted 24 hour time
