module Main (main) where

import Control.Monad ( replicateM )

append :: String -> String -> String
append string string' = undefined

delete :: Int -> String -> String
delete numberOfChars string = undefined

print' :: String -> IO ()
print' string = undefined

undo :: [String] -> (String, [String])
undo previousStringStates = undefined

evaluateOperations :: [(Int, Maybe String)] -> IO ()
evaluateOperations operations = undefined

main :: IO ()
main = do
  numberOfOperations <- (readLn :: IO Int) -- Read and bind number of operations to be entered
  operations <- replicateM numberOfOperations $ do
    (operationType : argument) <- words <$> getLine -- Read and bind operation type and potential argument
    let operationType' = read operationType :: Int -- Operation type as an integer
    let argument'      = head argument -- Potential argument (not evaluated unless later needed)
    case operationType' of
      -- Operation is 4 (undo) and therefore has no argument
      4 -> return (operationType', Nothing)
      -- Operation has an argument
      _ -> return (operationType', Just argument')
  
  evaluateOperations operations
  
  
