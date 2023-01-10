module Main (main) where

import Control.Monad ( replicateM )

-- factors n = factors' 1
--   where factors' x
--           | x < n     = x : [factors' + 1]
--           | otherwise = []
          -- | x == n = []
          -- | otherwise = x : [factors' + 1]

-- factors :: Int -> [Int]
-- factors :: (Ord a, Num a) => a -> [a]
-- factors n 
--   | n <= 1    = [1]
--   | otherwise = factors ((n `div` 2) - 1) ++ [n `div` 2]

-- factors :: (Floating a, Integral a) => a -> [a]
-- factors :: (RealFrac b, Floating b, Integral b) => b -> [b]
factors n = factors' 1
        -- Whether a given number is a factor of another number
  where -- isAFactorOf :: Integral a => a -> a -> Bool
        isAFactorOf a n = n `mod` a == 0
        -- The squareroot of given n
        -- sqrtN :: Floating a => a
        -- sqrtN :: Integral a => a
        sqrtN = round $ sqrt n
        -- The factors of given n starting from given a
        -- factors' :: (RealFrac b, Floating b, Integral b) => b -> [b]
        factors' a
          -- Given a is equal to the squareroot of given n (There cannot be any more factors)
          | a == sqrtN = []
          | otherwise  = if a `isAFactorOf` n -- Check if given a is a factor of given n
                           then [a] ++ factors' (a + 1) ++ [n `div` a] -- If so, add given a to the front of factors list, the rest of the factors in between, and a's opposite factor to the end
                           else factors' (a + 1) -- If not, check the next factor
                           

main :: IO ()
main = do
  numberOfQueries <- readLn :: IO Int -- Read and bind number of queries to be entered
  queries <- replicateM numberOfQueries $ do -- Replicate the following action for each query
    readLn :: IO Int -- Read query
  print queries