module Main (main) where

import qualified Data.Set as S
import Data.List
  ( sort )
import Control.Monad
  ( replicateM
  , mapM_ )
import Data.Maybe
  ( maybe )

main :: IO ()
main = do
  _ <- readLn :: IO Int -- Read but don't bind the upcoming list size
  integers <- (read <$>) . words <$> getLine :: IO [Int] -- Read and bind a list of integers
  let integerTotalsSet = S.fromList $ scanl (+) 0 $ reverse $ sort integers -- A set of totals when summing the given integers starting from the highest

  numberOfTestCases <- readLn :: IO Int -- Read and bind the number of test cases to be entered

  subsetSums <- replicateM numberOfTestCases $ do -- For each testcase do the following and bind the list of results
    integer <- readLn :: IO Int -- Read and bind the integer to find whether a non-empty subset from integers is greater than or equal to
    return $ maybe (- 1) (`S.findIndex` integerTotalsSet) (S.lookupGE integer integerTotalsSet) -- Return either -1 or the index of the lowest element greater than or equal to the given integer (if there exists one)

  mapM_ print subsetSums -- Print the subset sums of the entered list of integers and each entered number