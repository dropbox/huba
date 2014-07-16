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
import Control.Applicative ((<$>))
import System.Environment (getArgs)

import System.Log.Logger
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple
import System.Log.Formatter
import System.IO (stderr)

main :: IO ()
main = do
  setupLogging

  [basePortStr] <- getArgs
  let basePort = fromIntegral (read basePortStr :: Int)
      ingestorPort = basePort
      rootAggregatorPort = basePort + 1
      intermediateAggregatorPort = basePort + 2
      leafNodePort = basePort + 3

  noticeM "Main" $ "Starting the Ingestor on port " ++ show ingestorPort
  ingestor <- async $ do
    ingestorHandler <- newIngestorHandler
    void $ runBasicServer ingestorHandler IngestorService.process ingestorPort

  noticeM "Main" $ "Starting the Root Aggregator on port " ++ show rootAggregatorPort
  rootAggregator <- async $ do
    rootAggregatorHandler <- newRootAggregator
    void $ runBasicServer rootAggregatorHandler AggregatorService.process rootAggregatorPort

  noticeM "Main" $ "Starting the Intermediate Aggregator on port " ++ show intermediateAggregatorPort
  intermediateAggregator <- async $ do
    intermediateAggregatorHandler <- newIntermediateAggregator [Server "localhost" leafNodePort] -- TODO: this is hacky
    void $ runBasicServer intermediateAggregatorHandler InternalAggregatorService.process intermediateAggregatorPort

  -- TODO: start $ncpu LeafNodes
  noticeM "Main" $ "Starting a Leaf Node on port " ++ show leafNodePort
  leafNode <- async $ do
    leafHandler <- newLeafNodeHandler
    void $ runBasicServer leafHandler LeafNodeService.process leafNodePort

  _ <- waitAny [ingestor, rootAggregator, intermediateAggregator, leafNode]
  return ()


setupLogging :: IO ()
setupLogging = do
  let format = simpleLogFormatter "[$loggername] $pid | $tid: $msg"
  handler <- (`setFormatter` format) <$> streamHandler stderr DEBUG
  updateGlobalLogger rootLoggerName (setHandlers [handler] . setLevel DEBUG)
