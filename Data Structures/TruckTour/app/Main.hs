module Main (main) where

import Control.Monad ( replicateM )

-- Datatype for a petrol pump with an amount of petrol available and the distance to the next pump
data PetrolPump =
  PetrolPump
    { amountOfPetrolAvailable :: Int
    , distanceToNextPump      :: Int
    } deriving ( Show )

-- Whether a tour can be completed when starting with a given amount of petrol in the tank and from the first petrol pump in a given list
-- Example: tourCanBeCompleted 0 [PetrolPump 10 5, PetrolPump 0 5] = True
tourCanBeCompleted :: Int -> [PetrolPump] -> Bool
tourCanBeCompleted _ [] = True
tourCanBeCompleted amountOfPetrolInTank (petrolPump : restOfPetrolPumps)
  | usedPetrolTank < 0 = False
  | otherwise          = tourCanBeCompleted usedPetrolTank restOfPetrolPumps
  where
    filledPetrolTank = amountOfPetrolInTank + amountOfPetrolAvailable petrolPump
    usedPetrolTank   = filledPetrolTank - distanceToNextPump petrolPump

-- The index of the first petrol pump from a given list of petrol pumps that can complete the tour (won't run out of petrol)
-- Example: firstPetrolPumpThatCanCompleteTour [PetrolPump 1 5, PetrolPump 10 3, PetrolPump 3 4] = 1
-- Note: This function can run infinitely if there is not a petrol pump that can complete the tour
firstPetrolPumpThatCanCompleteTour :: [PetrolPump] -> Int
firstPetrolPumpThatCanCompleteTour petrolPumps@(petrolPump : restOfPetrolPumps)
  | tourCanBeCompleted 0 petrolPumps = 0
  | otherwise                        = 1 + firstPetrolPumpThatCanCompleteTour (restOfPetrolPumps ++ [petrolPump])


main :: IO ()
main = do
  numberOfPetrolPumps <- readLn :: IO Int -- Read and bind number of petrol pumps to be entered
  petrolPumps <- replicateM numberOfPetrolPumps $ do -- For each petrol pump to be entered...
    (amountOfPetrolAvailable : distanceToNextPump : _) <- (read <$>) . words <$> getLine :: IO [Int] -- Read amount of petrol to give and distance to next pump
    return $ PetrolPump amountOfPetrolAvailable distanceToNextPump -- Return entered petrol pump

  print $ firstPetrolPumpThatCanCompleteTour petrolPumps -- Print the index of the first petrol pump that can complete the tour from the list of entered petrol pumps