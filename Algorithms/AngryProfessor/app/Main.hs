module Main (main) where

import Control.Monad ( replicateM
                     , replicateM_ )

-- Whether or not the class is canceled depending on the cancelation threshold and arrival times of the students
angryProfessor :: Int -> [Int] -> String
angryProfessor cancelationThreshold arrivalTimes
  -- The number of students who arrived on time is greater than or equal to the cancelation threshold
  | (length . filter (== True)) (map (<= 0) arrivalTimes) >= cancelationThreshold = "NO" -- Class not canceled
  -- Otherwise (the number of students who arrived on time is less than the cancelation threshold)
  | otherwise                                                                     = "YES" -- Class canceled

main :: IO ()
main = do
  numberOfTestCases <- readLn :: IO Int -- Read number of test cases to be entered and bind it
  testCases <- replicateM numberOfTestCases $ -- Replicate the following action for each test case
    do
      (_: cancelationThreshold:_) <- map read . words <$> getLine :: IO [Int] -- Read the cancelation threshold and bind it (ignores first int entered and any after that)
      arrivalTimes <- map read . words <$> getLine :: IO [Int] -- Read the list of arrival times and bind it
      return (cancelationThreshold, arrivalTimes) -- Return the cancelation threshold and arrival times as a tuple
  mapM_ (\(cancelationThreshold, arrivalTimes) -> putStrLn $ angryProfessor cancelationThreshold arrivalTimes) testCases -- Print which classes are canceled or not
