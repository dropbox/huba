module Main where

import Launch (runServer)
import System.Environment (getArgs)
import Control.Concurrent.Async (waitAny)

main :: IO ()
main = do
  [basePortStr] <- getArgs
  let basePort = fromIntegral (read basePortStr :: Int)
      ingestorPort = basePort
      rootAggregatorPort = basePort + 1
      intermediateAggregatorPort = basePort + 2
      leafNodePort = basePort + 3

  processes <- runServer (ingestorPort, rootAggregatorPort, intermediateAggregatorPort, leafNodePort)

  _ <- waitAny processes

  return ()
