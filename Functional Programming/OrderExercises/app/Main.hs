module Main (main) where

-- import qualified Data.Sequence as Seq
-- import qualified Data.Array as Arr
import qualified Data.Vector as Vec
import Data.Maybe
import Data.List
  ( intercalate )
import Data.Foldable
  ( toList )

kadaneBestSumAndLeftovers currentSum (x, y) bestSum (x', y') numbers i =
  if i == Vec.length numbers
  then
    (bestSum, leftovers)
  else
    kadaneBestSumAndLeftovers newCurrentSum (newX, newY) newBestSum (newX', newY') numbers (i + 1)
  where
    number = numbers Vec.! i
    (newCurrentSum, (newX, newY)) =
      if number > (currentSum + number) then
        (number, (i, i))
      else
        (currentSum + number, (x, i))
    (newBestSum, (newX', newY')) =
      if newCurrentSum > bestSum then
        (newCurrentSum, (newX, newY))
      else
        (bestSum, (x', y'))
    leftovers = Vec.take x' numbers <> Vec.drop (y' + 1) numbers

sequenceSums numbers maximumNumberOfSums
  -- The maximumNumber of sums has been reached or there are no numbers in given list or the maximum sum sequence is negative
  | maximumNumberOfSums == 0 || Vec.null numbers || bestSum == 0 = []
  | otherwise                                                    = bestSum : sequenceSums leftovers (maximumNumberOfSums - 1)
  where
    -- The maximum sum sequence along with the leftover integer list
    (bestSum, leftovers) = kadaneBestSumAndLeftovers 0 (0, 0) 0 (0, 0) numbers 0

-- kadaneBestSumAndLeftovers currentSum currentSequence bestSum bestSequence vNumbers =
--   if isNothing maybeNumberAndNumbers then
--     (bestSum, bestSequence)
--   else
--     kadaneBestSumAndLeftovers newCurrentSum newCurrentSequence newBestSum newBestSequence numbers
--   where
--     maybeNumberAndNumbers = Vec.uncons vNumbers
--     (number, numbers) = fromJust maybeNumberAndNumbers
--     -- The new current sum and sequence
--     (newCurrentSum, newCurrentSequence) =
--       if number > (currentSum + number) then
--         (number, Vec.singleton number)
--       else
--         (currentSum + number, currentSequence `Vec.snoc` number)
--     -- The new best sum and sequence
--     (newBestSum, newBestSequence) =
--       if newCurrentSum > bestSum then
--         (newCurrentSum, newCurrentSequence)
--       else
--         (bestSum, bestSequence)

-- -- A given list (y : ys) with the first sequence of (x : xs) removed
-- -- Ex: removeFirstSubsequence (Seq.fromList [1,2]) (Seq.fromList [1,2,3,4]) Seq.empty = fromList [3,4]
-- removeFirstSubsequence :: Eq a => Seq.Seq a -> Seq.Seq a -> Seq.Seq a -> Seq.Seq a
-- removeFirstSubsequence (x Seq.:<| xs) (y Seq.:<| ys) matches
--   -- Current elements are the same
--   | x == y       = removeFirstSubsequence xs ys (matches Seq.:|> x)
--   -- There are no previous matches
--   | null matches = y Seq.:<| removeFirstSubsequence (x Seq.:<| xs) ys Seq.empty
--   -- Otherwise the current elements are different and there are no previous matches
--   | otherwise    = matches Seq.>< removeFirstSubsequence (matches Seq.>< (x Seq.:<| xs)) (y Seq.:<| ys) Seq.empty
-- -- There is no xs
-- removeFirstSubsequence Seq.Empty ys _ = ys
-- -- There is no ys
-- removeFirstSubsequence _ Seq.Empty matches = matches

-- -- The best subsequence sum and the subsequence itself of a given list of numbers
-- -- Ex: kadaneBestSumAndSequence 0 Seq.empty 0 Seq.empty (Seq.fromList [1,-2,5]) = (5,fromList [5])
-- kadaneBestSumAndSequence :: (Num a, Ord a) => a -> Seq.Seq a -> a -> Seq.Seq a -> Seq.Seq a -> (a, Seq.Seq a)
-- kadaneBestSumAndSequence currentSum currentSequence bestSum bestSequence (number Seq.:<| numbers) = kadaneBestSumAndSequence newCurrentSum newCurrentSequence newBestSum newBestSequence numbers
--   where
--     -- The new current sum and sequence
--     (newCurrentSum, newCurrentSequence)
--       = if number > (currentSum + number)
--         then
--           (number, Seq.singleton number)
--         else
--           (currentSum + number, currentSequence Seq.:|> number)
--     -- The new best sum and sequence
--     (newBestSum, newBestSequence)
--       = if newCurrentSum > bestSum
--           then
--             (newCurrentSum, newCurrentSequence)
--           else
--             (bestSum, bestSequence)
-- -- There are no given numbers
-- kadaneBestSumAndSequence _ _ bestSum bestSequence Seq.Empty = (bestSum, bestSequence)

-- -- The best subsequence sum and leftover elements of a list of numbers
-- -- kadaneMaxSumAndLeftovers (Seq.fromList [1,-2,5]) = (5,fromList [1,-2])
-- kadaneMaxSumAndLeftovers :: (Num a, Ord a) => Seq.Seq a -> (a, Seq.Seq a)
-- kadaneMaxSumAndLeftovers numbers = (bestSum, leftovers)
--   where
--     (bestSum, bestSequence) = kadaneBestSumAndSequence 0 Seq.empty 0 Seq.empty numbers
--     leftovers               = removeFirstSubsequence bestSequence numbers Seq.empty

-- -- A list of maximum sum sequences of a given list of numbers (up to the size of a given maximumNumberOfSums)
-- -- Ex: sequenceSums (Seq.fromList [1,2,3]) 2 = fromList [6]
-- -- sequenceSums :: (Ord a, Num t, Num a, Eq t) => [a] -> t -> [a]
-- sequenceSums :: (Num t, Num a, Ord a, Eq t) => Seq.Seq a -> t -> Seq.Seq a
-- sequenceSums numbers maximumNumberOfSums
--   -- The maximumNumber of sums has been reached or there are no numbers in given list or the maximum sum sequence is negative
--   | maximumNumberOfSums == 0 || null numbers || maximumSumSequence == 0 = Seq.empty
--   | otherwise                                                           = maximumSumSequence Seq.:<| sequenceSums leftovers (maximumNumberOfSums - 1)
--   where
--     -- The maximum sum sequence along with the leftover integer list
--     (maximumSumSequence, leftovers) = kadaneMaxSumAndLeftovers numbers

main :: IO ()
main = do
  (_ : maximumNumberOfSums : _) <- (read <$>) . words <$> getLine :: IO [Int] -- Read and bind the maximum number of sums to output (ignore the number of numbers to be entered as we do not need it)
  numbers <- (read <$>) . words <$> getLine :: IO [Int] -- Read and bind the list of numbers to calculate the list of sequence of sums of
  putStrLn $ intercalate "\n" $ show <$> toList (sequenceSums (Vec.fromList numbers) maximumNumberOfSums) -- Print the sequence of sums of a given list of numbers (up to the size of a given maximumNumberOfSums)
