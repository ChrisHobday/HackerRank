module Main (main) where

import Control.Monad ( replicateM )

-- Convert a given "color" char to an number (Used to track the difference between color pairs in sequenceFullOfColors function)
-- Ex : colorToNumber 'R' = 1
colorToNumber :: Num a => Char -> a
colorToNumber color
  | color == 'R' = 1
  | color == 'G' = -1
  | color == 'Y' = 1
  | color == 'B' = -1

-- Whether or not a given string is a proper "sequence full of colors"
-- Ex : sequenceFullOfColors "RYBG" = True
sequenceFullOfColors :: [Char] -> Bool
sequenceFullOfColors colors = sequenceFullOfColors' 0 0 colors
  where
    -- Subfunction in order to encapsulate the redGreenDifference and yellowBlueDifference
    sequenceFullOfColors' :: (Ord a1, Ord a2, Num a2, Num a1) => a1 -> a2 -> [Char] -> Bool
    -- There is at least 1 color in the given String of colors
    sequenceFullOfColors' redGreenDifference yellowBlueDifference (color : colors)
      -- Color is either red or green
      | color == 'R' || color == 'G' = if abs newRedGreenDifference > 1 -- The new difference between red and green balls is greater than 1
                                        then False
                                        else sequenceFullOfColors' newRedGreenDifference yellowBlueDifference colors
      -- Color is either yellow or blue
      | color == 'Y' || color == 'B' = if abs newYellowBlueDifference > 1 -- The new difference between yellow and blue balls is greater than 1
                                        then False
                                        else sequenceFullOfColors' redGreenDifference newYellowBlueDifference colors
      where
        newRedGreenDifference   = redGreenDifference + colorToNumber color
        newYellowBlueDifference = yellowBlueDifference + colorToNumber color
    -- There is not at least 1 color in the given String of colors
    sequenceFullOfColors' redGreenDifference yellowBlueDifference _
      -- There are as many red balls as green balls and there are as many yellow balls as blue balls.
      | redGreenDifference == 0 && yellowBlueDifference == 0 = True
      -- Otherwise (There are not as many red balls as green balls or there are not as many yellow balls as blue balls.)
      | otherwise                                            = False

main :: IO ()
main = do
  numberOfTestCases <- readLn :: IO Int -- Read number of test cases to be entered and bind it
  testCases <- replicateM numberOfTestCases $ do -- Replicate the following action for each test case
    string <- getLine -- Read and bind the string of colors to determine is a sequence full of colors
    return $ sequenceFullOfColors string -- Return whether user entered colors is a sequence full of colors
  mapM_ print testCases -- Print whether each of the entered list of colors was a sequence full of colors or not (one per line)
