{-# LANGUAGE ScopedTypeVariables #-}

module Launch (runServer) where

import Ingestor.Server
import qualified IngestorService

import LeafNode.Server
import qualified LeafNodeService

import Aggregator.Server
import qualified AggregatorService
import qualified InternalAggregatorService

-- import Thrift
import Thrift.Transport.Handle
import Thrift.Protocol.Binary
import Control.Concurrent ( forkIO )

import Shared.Thrift.ClientInterface (Server(..))

import Control.Concurrent.Async (async, waitAny, Async)
import Control.Monad
import Control.Applicative ((<$>))
import Control.Exception (catch, SomeException, handle, bracket, AsyncException)

import System.Log.Logger
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple
import System.Log.Formatter
import System.IO

import Network


runServer :: (PortNumber, PortNumber, PortNumber, PortNumber) -> IO [Async ()]
runServer (ingestorPort, rootAggregatorPort, intermediateAggregatorPort, leafNodePort) = do
  setupLogging

  noticeM "Main" $ "Starting the Ingestor on port " ++ show ingestorPort
  ingestor <- async $ do
    ingestorHandler <- newIngestorHandler
    catch (runBasicServer ingestorHandler IngestorService.process ingestorPort)
          (handleFailure "Ingestor")

  noticeM "Main" $ "Starting the Root Aggregator on port " ++ show rootAggregatorPort
  rootAggregator <- async $ do
    rootAggregatorHandler <- newRootAggregator
    void $ catch (runBasicServer rootAggregatorHandler AggregatorService.process rootAggregatorPort)
                 (handleFailure "Root Aggregator")

  noticeM "Main" $ "Starting the Intermediate Aggregator on port " ++ show intermediateAggregatorPort
  intermediateAggregator <- async $ do
    intermediateAggregatorHandler <- newIntermediateAggregator [Server "localhost" leafNodePort] -- TODO: this is hacky
    void $ catch (runBasicServer intermediateAggregatorHandler InternalAggregatorService.process intermediateAggregatorPort)
                 (handleFailure "Intermediate Aggregator")

  -- TODO: start $ncpu LeafNodes
  noticeM "Main" $ "Starting a Leaf Node on port " ++ show leafNodePort
  leafNode <- async $ do
    leafHandler <- newLeafNodeHandler
    void $ catch (runBasicServer leafHandler LeafNodeService.process leafNodePort)
                 (handleFailure "Leaf Node")

  return [ingestor, rootAggregator, intermediateAggregator, leafNode]


handleFailure :: String -> AsyncException -> IO ()
handleFailure service e = do
  noticeM "runServer" $ "Exception in " ++ show service ++ " (" ++ (show e) ++ ")"


setupLogging :: IO ()
setupLogging = do
  let format = simpleLogFormatter "[$loggername] $pid | $tid: $msg"
  handler <- (`setFormatter` format) <$> streamHandler stderr DEBUG
  updateGlobalLogger rootLoggerName (setHandlers [handler] . setLevel DEBUG)



------------------------------------------------------------------------------------------------


-- | A threaded sever that is capable of using any Transport or Protocol
-- instances.
runThreadedServer :: (Transport t, Protocol i, Protocol o)
                  => (Socket -> IO (i t, o t))
                  -> h
                  -> (h -> (i t, o t) -> IO Bool)
                  -> PortID
                  -> IO a
runThreadedServer accepter hand proc port = do
    bracket (listenOn port)
            (\x -> do
               noticeM "ACTUALLY CLOSING" "**********************************************************"
               sClose x)
            (\x -> acceptLoop (accepter x) (proc hand))

-- | A basic threaded binary protocol socket server.
runBasicServer :: h
               -> (h -> (BinaryProtocol Handle, BinaryProtocol Handle) -> IO Bool)
               -> PortNumber
               -> IO a
runBasicServer hand proc port = runThreadedServer binaryAccept hand proc (PortNumber port)
  where binaryAccept s = do
            (h, _, _) <- accept s
            return (BinaryProtocol h, BinaryProtocol h)

acceptLoop :: IO t -> (t -> IO Bool) -> IO a
acceptLoop accepter proc = forever $
    do ps <- accepter
       forkIO $ handle (\(e :: SomeException) -> return ())
                       (loop $ proc ps)
  where loop m = do { continue <- m; when continue (loop m) }
