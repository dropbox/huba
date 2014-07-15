module Main where

import Ingestor.Server
import qualified IngestorService

import LeafNode.Server
import qualified LeafNodeService

import Thrift.Server
import Control.Concurrent.Async (async, waitAny)


main :: IO ()
main = do
  ingestor <- async $ do
    ingestorHandler <- newIngestorHandler
    putStrLn "Starting the Ingestor..."
    _ <- runBasicServer ingestorHandler IngestorService.process 9090
    putStrLn "done."

  leafNode <- async $ do
    leafHandler <- newLeafNodeHandler
    putStrLn "Starting the Leaf Nodes..."
    _ <- runBasicServer leafHandler LeafNodeService.process 9091
    putStrLn "done."

  _ <- waitAny [ingestor, leafNode]
  return ()
