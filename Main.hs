module Main where

import Ingestor
import qualified IngestorService

import Thrift.Server


main :: IO ()
main =  do
  handler <- newIngestorHandler
  putStrLn "Starting the Ingestor..."
  _ <- runBasicServer handler IngestorService.process 9090
  putStrLn "done."

  putStrLn "Starting the Leaf Nodes..."
