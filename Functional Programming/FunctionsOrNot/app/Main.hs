module Main (main) where

import Control.Monad ( replicateM )

isFunction :: [(Int, Int)] -> String
isFunction pairs = undefined

main :: IO ()
main = do
  numberOfTestCasesToBeEntered <- readLn :: IO Int -- Read and bind number of test cases to be entered

  testCases <- replicateM numberOfTestCasesToBeEntered $ do -- For each test case to be entered...

    numberOfPairsToBeEntered <- readLn :: IO Int -- Read and bind number of pairs to be entered

    replicateM numberOfPairsToBeEntered $ do -- For each pair to be entered...

      (x : y : _) <- (read <$>) . words <$> getLine :: IO [Int] -- Read and bind the pair to x and y
      return (x, y) -- Return the pair as (x, y)

  mapM_ (putStrLn . isFunction) testCases -- Print whether or not each test case is a function
