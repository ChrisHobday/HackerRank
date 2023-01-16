module Main (main) where

import Control.Monad ( replicateM
                     , forM_
                     , mapM_ )
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

-- Note: The trick to this problem, once you wrap your head around what it's asking you to do in the problem statement.
-- 1. Realise

mutablePrecomputed n = do
  let intMax = maxBound :: Int
  mv <- MV.replicate (n + 1) intMax
  -- mv <- MV.new (n + 1)
  -- mv <- MV.generate (n + 1) id -- Create and bind mutable vector of n + 1 size, each index filled with it's own value (Example: MV.generate 10 id = [0,1,2,3,4,5,6,7,8,9])

  MV.write mv 0 0
  MV.write mv 1 1
  MV.write mv 2 2

  forM_ [2 .. n] ( \i -> do -- For each number from 2 to n
    currentValue  <- MV.read mv i -- Bind current value at index i
    previousValue <- MV.read mv (i - 1) -- Bind previous value at index i - 1
    MV.write mv i (min currentValue (previousValue + 1)) -- Write min of current value or previous value + 1 to mutable vector index i
    let nDivValue = min (n `div` i) i
    forM_ [2 .. nDivValue] (\j -> do -- For each number from 2 to 
      currentValue2 <- MV.read mv (i * j) -- Bind current value 2 at index i * j
      factorValue   <- MV.read mv i -- Bind factor value at index i
      MV.write mv (i * j) (min currentValue2 (factorValue + 1)) -- Write min of current value 2 or factor value + 1 to mutable vectors index i * j
      )
    )

  V.freeze mv

main :: IO ()
main = do
  numberOfQueries <- readLn :: IO Int -- Read and bind number of queries to be entered
  queries <- replicateM numberOfQueries $ do -- Replicate the following action for each query
    readLn :: IO Int -- Read query

  v <- mutablePrecomputed (maximum queries) -- Create and bind a vector of smallest number of steps to zero for each number from 1 to the maximum queried

  mapM_ (print . (v V.!)) queries