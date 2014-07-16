module Main where

import Ingestor.Server
import qualified IngestorService

import LeafNode.Server
import qualified LeafNodeService

import Aggregator.Server
import qualified AggregatorService
import qualified InternalAggregatorService

import Thrift.Server

import Shared.Thrift.ClientInterface (Server(..))

import Control.Concurrent.Async (async, waitAny)
import Control.Monad
import System.Environment (getArgs)


main :: IO ()
main = do
  [basePortStr] <- getArgs
  let basePort = fromIntegral (read basePortStr :: Int)
      ingestorPort = basePort
      rootAggregatorPort = basePort + 1
      intermediateAggregatorPort = basePort + 2
      leafNodePort = basePort + 3

  putStrLn $ "Starting the Ingestor on port " ++ show ingestorPort
  ingestor <- async $ do
    ingestorHandler <- newIngestorHandler
    void $ runBasicServer ingestorHandler IngestorService.process ingestorPort

  putStrLn $ "Starting the Root Aggregator on port " ++ show rootAggregatorPort
  rootAggregator <- async $ do
    rootAggregatorHandler <- newRootAggregator
    void $ runBasicServer rootAggregatorHandler AggregatorService.process rootAggregatorPort

  putStrLn $ "Starting the Intermediate Aggregator on port " ++ show intermediateAggregatorPort
  intermediateAggregator <- async $ do
    intermediateAggregatorHandler <- newIntermediateAggregator [Server "localhost" leafNodePort] -- TODO: this is hacky
    void $ runBasicServer intermediateAggregatorHandler InternalAggregatorService.process intermediateAggregatorPort

  -- TODO: start $ncpu LeafNodes
  putStrLn $ "Starting a Leaf Node on port " ++ show leafNodePort
  leafNode <- async $ do
    leafHandler <- newLeafNodeHandler
    void $ runBasicServer leafHandler LeafNodeService.process leafNodePort

  _ <- waitAny [ingestor, rootAggregator, intermediateAggregator, leafNode]
  return ()
