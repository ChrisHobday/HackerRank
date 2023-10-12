module Main (main) where

import Control.Monad ( replicateM )
import Data.Array
  ( Array
  , array
  , listArray
  , bounds
  , (!)
  )
-- import Data.Vector

-- test pat = table
--   where
--     patLength = length pat
--     patArray  = listArray (0, patLength - 1) pat
--     table     = array (0, patLength - 1) (
--       let gen j i previousMatch
--             | i == patLength               = []
--             | patArray ! j /= patArray ! i =
--               if not previousMatch
--                 then (i, 0) : gen j (i + 1) False
--                 -- else gen 0 7 False
--                 -- else [(i, (table ! (j - 1)))]
--                 else 
--                   if patArray ! (table ! (j - 1)) == patArray ! i
--                     then [(i, 999)]
--                     else [(i, 888)]

--                   -- gen (table ! (j - 1)) i False
--             | otherwise                    = (i, j + 1) : gen (j + 1) (i + 1) True
--             where
--               previousJ = table ! (j - 1)
--       in
--         if patLength - 1 < 0
--           then []
--           else (0, 0) : gen 0 1 False)

test pat = table
  where
    patLength = length pat
    patArray  = listArray (0, patLength - 1) pat
    table     = array (0, patLength - 1) (
      let gen j i previousMatch
            | i == patLength               = []
            | patArray ! j /= patArray ! i =
              if not previousMatch
                then (i, 0) : gen j (i + 1) False
                -- else gen 0 7 False
                -- else [(i, (table ! (j - 1)))]
                else 
                  if patArray ! (table ! (j - 1)) == patArray ! i
                    then [(i, 999)]
                    else [(i, 888)]

                  -- gen (table ! (j - 1)) i False
            | otherwise                    = (i, j + 1) : gen (j + 1) (i + 1) True
      in
        if patLength - 1 < 0
          then []
          else (0, 0) : gen 0 1 False)

-- -- |The solid data type of KMP table
data Table a = Table
  { alphabetTable :: Array Int a
  , jumpTable :: Array Int Int
  , len :: Int
  } deriving (Show)

build pat =
  let
    len = length pat

    resTable = Table
      { alphabetTable = listArray (0,len-1) pat
      , jumpTable = listArray (-1,len-1) $ (-2) : genJump (-1) 0
      , len = len
      }

    genJump _ 0 =
      let
        o = if 1 == len || alphabetTable resTable ! 0 /= alphabetTable resTable ! 1
          then -1
          else -2

        later = genJump (-1) 1
      in
        o : later

    genJump lastMPJump i =
      let
        ch = alphabetTable resTable ! i

        findJ oldJ
          | oldJ == -2 = -2
          -- Checking if next j character is same as current i character
          | alphabetTable resTable ! (oldJ+1) == ch = oldJ
          | oldJ == -1 = -2

          | otherwise = findJ (jumpTable resTable ! oldJ)

        j = findJ lastMPJump

        o = if i+1 == len || alphabetTable resTable ! (i+1) /= alphabetTable resTable ! (j+2)
          then j+1
          else jumpTable resTable ! (j+1)

        later = genJump (j+1) (i+1)
      in o : later

  in
    resTable

kmp text pat = "No"

main :: IO ()
main = do
  testCases <- readLn :: IO Int -- Read and bind the number of test cases to be entered
  subtringMatches <- replicateM testCases $ do -- Replicate the following action the given number of test cases times
    text <- getLine -- Read and bind the text string to search for the given pattern in
    pat <- getLine -- Read and bind the pattern to search the given text string for
    return $ kmp text pat -- Return whether the givne pat exists in the given text string (using kmp substring search)
  mapM_ putStrLn subtringMatches -- Print whether each test cases given pattern exists in each test cases given text string (each on a seperate line)
