module Main (main) where

import Control.Monad ( replicateM
                     , forM_
                     , mapM_ )
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

-- Note: The trick to this problem, once you wrap your head around what it's asking you to do in the problem statement.
-- 1. Realise the use of factors/prime numbers
-- 2. Prebuild entire list of smallest number of steps to zero for all values 1-1000001 starting at 1
-- 3. Use mutable vectors for speed (Lists and even mutable vectors were not fast enough for hackerrank time limit)

mutablePrecomputed n = do
  let maxInt = maxBound :: Int -- Maximum int size

  mv <- MV.replicate (n + 1) maxInt -- Create and bind mutable vector of size n + 1 filled with max int

  -- Write 0,1,2 as first 3 values of mutable vector
  MV.write mv 0 0
  MV.write mv 1 1
  MV.write mv 2 2

  forM_ [2 .. n] ( \i -> do -- For each number from 2 to n
    currentValue  <- MV.read mv i -- Bind current value at index i
    previousValue <- MV.read mv (i - 1) -- Bind previous value at index i - 1
    MV.write mv i (min currentValue (previousValue + 1)) -- Write min of current value or previous value + 1 to mutable vector index i
    let nDivValue = min i (n `div` i) -- The number of possible upcoming vector positions this position could be jumped to from
    forM_ [2 .. nDivValue] (\j -> do -- For each number from 2 to n div value
      currentValue2 <- MV.read mv (i * j) -- Bind current value 2 at index i * j
      factorValue   <- MV.read mv i -- Bind factor value at index i
      MV.write mv (i * j) (min currentValue2 (factorValue + 1)) -- Write min of current value 2 or factor value + 1 to mutable vectors index i * j
      )
    )

  V.freeze mv -- Convert mutable vector into immutable vector to be used outside this monad

main :: IO ()
main = do
  numberOfQueries <- readLn :: IO Int -- Read and bind number of queries to be entered
  queries <- replicateM numberOfQueries $ do -- Replicate the following action for each query
    readLn :: IO Int -- Read query

  v <- mutablePrecomputed 1000001 -- Create and bind a vector of smallest number of steps to zero for each number from 1 to 1000001

  mapM_ (print . (v V.!)) queries -- Print the value at 