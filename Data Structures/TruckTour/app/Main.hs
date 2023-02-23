module Main (main) where

import Control.Monad ( replicateM )
import qualified Data.Sequence as S

-- Datatype for a petrol pump with an amount of petrol available and the distance to the next pump
data PetrolPump =
  PetrolPump
    { amountOfPetrolAvailable :: Int
    , distanceToNextPump      :: Int
    } deriving ( Show )

-- Whether a tour can be completed when starting with a given amount of petrol in the tank and from the first petrol pump in a given list
-- Example: tourCanBeCompleted 0 [PetrolPump 10 5, PetrolPump 0 5] = True
tourCanBeCompleted :: Int -> S.Seq PetrolPump -> Bool
-- There is no more petrol pumps to visit (the tour is complete)
tourCanBeCompleted _ S.Empty = True
tourCanBeCompleted amountOfPetrolInTank (petrolPump S.:<| restOfPetrolPumps)
  -- The truck will run out of gas before the next petrol pump
  | usedPetrolTank < 0 = False
  -- The truck will make it to the next petrol pump
  | otherwise          = tourCanBeCompleted usedPetrolTank restOfPetrolPumps
  where
    -- The amount of petrol in the tank after being filled with available petrol at current pump
    filledPetrolTank = amountOfPetrolInTank + amountOfPetrolAvailable petrolPump
    -- The amount of petrol left after filled tank is driven to next petrol pump
    usedPetrolTank   = filledPetrolTank - distanceToNextPump petrolPump

-- The index of the first petrol pump from a given list of petrol pumps that can complete the tour (won't run out of petrol)
-- Example: firstPetrolPumpThatCanCompleteTour [PetrolPump 1 5, PetrolPump 10 3, PetrolPump 3 4] = 1
-- Note: This function can run infinitely if there is not a petrol pump that can complete the tour
firstPetrolPumpThatCanCompleteTour :: S.Seq PetrolPump -> Int
firstPetrolPumpThatCanCompleteTour petrolPumps@(petrolPump S.:<| restOfPetrolPumps)
  -- The tour can be completed from the current petrol pump
  | tourCanBeCompleted 0 petrolPumps = 0
  -- The tour cannot be completed from the current petrol pump
  | otherwise                        = 1 + firstPetrolPumpThatCanCompleteTour (restOfPetrolPumps S.|> petrolPump)


main :: IO ()
main = do
  numberOfPetrolPumps <- readLn :: IO Int -- Read and bind number of petrol pumps to be entered
  petrolPumps <- replicateM numberOfPetrolPumps $ do -- For each petrol pump to be entered...
    (amountOfPetrolAvailable : distanceToNextPump : _) <- (read <$>) . words <$> getLine :: IO [Int] -- Read amount of petrol to give and distance to next pump
    return $ PetrolPump amountOfPetrolAvailable distanceToNextPump -- Return entered petrol pump
  
  let petrolPumpsSequence = S.fromList petrolPumps -- A sequence of petrol pumps from the entered list

  print $ firstPetrolPumpThatCanCompleteTour petrolPumpsSequence -- Print the index of the first petrol pump that can complete the tour from the list of entered petrol pumps