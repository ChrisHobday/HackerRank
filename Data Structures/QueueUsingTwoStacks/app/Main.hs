module Main (main) where

import Control.Monad ( replicateM
                     , forM_ )

evaluateQuery 1 = undefined
evaluateQuery 2 = undefined
evaluateQuery 3 = undefined

enqueue value = undefined

dequeue = undefined

main :: IO ()
main = do
  numberOfQueries <- (readLn :: IO Int) -- Read and bind number of queries to be entered
  queries <- replicateM numberOfQueries $ do -- Replicate the following action for each query to be entered
    (queryType : _ : queryValue : _) <- getLine -- Read and bind query type and value
    return (queryType, queryValue) -- Return query type and value to list of queries

  forM_ queries (\query -> do -- For each query
    print query)
