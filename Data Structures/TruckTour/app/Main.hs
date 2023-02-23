module Main (main) where

import Control.Monad ( replicateM )

data PetrolPump =
  PetrolPump
    { amountOfPetrolToGive :: Int
    , distanceToNextPump   :: Int
    } deriving ( Show )

-- The index of the first petrol pump from a given list of petrol pumps that can complete the tour (won't run out of petrol)
firstPetrolPumpThatCanCompleteTour :: [PetrolPump] -> Int
firstPetrolPumpThatCanCompleteTour = undefined

main :: IO ()
main = do
  numberOfPetrolPumps <- readLn :: IO Int -- Read and bind number of petrol pumps to be entered
  petrolPumps <- replicateM numberOfPetrolPumps $ do -- For each petrol pump to be entered...
    (amountOfPetrolToGive : distanceToNextPump : _) <- (read <$>) . words <$> getLine :: IO [Int] -- Read amount of petrol to give and distance to next pump
    return $ PetrolPump amountOfPetrolToGive distanceToNextPump -- Return entered petrol pump

  print petrolPumps