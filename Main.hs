module Main where

import Ingestor.Server
import qualified IngestorService

import LeafNode.Server
import qualified LeafNodeService

import Thrift.Server
import Control.Concurrent.Thread (forkIO)


main :: IO ()
main = do
  (_, wait1) <- forkIO $ do
    ingestorHandler <- newIngestorHandler
    putStrLn "Starting the Ingestor..."
    _ <- runBasicServer ingestorHandler IngestorService.process 9090
    putStrLn "done."

  (_, wait2) <- forkIO $ do
    leafHandler <- newLeafNodeHandler
    putStrLn "Starting the Leaf Nodes..."
    _ <- runBasicServer leafHandler LeafNodeService.process 9091
    putStrLn "done."

  _ <- wait1
  _ <- wait2
  return ()
