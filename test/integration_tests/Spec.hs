-- module Spec (tests) where
module Main where

import qualified Huba_Types as T

import Distribution.TestSuite
import Launch (runServer)
import Testing.Util (genRandomLogMessage, waitForServer)

import Shared.Thrift.Interface
import Shared.Thrift.ClientInterface (Server(..))

import Thrift
import Thrift.Protocol.Binary
import Thrift.Transport.Handle

import qualified IngestorService_Client as IngestorClient

import Control.Concurrent.Async (async, waitAny, mapConcurrently)
import Control.Applicative ( (<$>) )
import Network
import Data.Foldable (toList)
import Data.Vector as V (fromList)
import Control.Monad (replicateM_, replicateM, forM, liftM)

import System.Timeout (timeout)

import qualified Data.Random.Source.DevRandom as R
import qualified Data.Random as R


tests :: IO [Test]
tests = do
  putStrLn "Running integration tests..."

  -- Start a number of servers
  let n = 5
      basePorts = take n $ iterate (+10) 8000
      portTuples = [(x, x+1, x+2, x+3) | x <- basePorts]

  processes <- concat <$> forM portTuples runServer

  -- TODO: ensure all ports respond to ping
  -- Launch a bunch of simultaneous green threads that do pings, and return when they're happy
  -- If any of them don't succeed within some timeout, fail the test
  let firstProcess = head processes
  pingResponses <- mapConcurrently
                   (\x -> timeout (5 * 10^6) (waitForServer $ Server "localhost" x))
                   [8000]

  -- Uncommenting this line lets us wait on all the processes
  -- _ <- waitAny $ concat processes
  -- waitAny [pings]

  -- Send a bunch of random LogMessages into the system
  transport <- hOpen ("localhost" :: String, PortNumber 8000)
  let binProto = BinaryProtocol transport
      protocols = (binProto, binProto)
      numBatches = 10
      batchSize = 10

  replicateM_ numBatches $ do
         -- Get batchSize random messages
         msgs <- replicateM batchSize $ R.runRVar genRandomLogMessage R.DevURandom
         resp <- IngestorClient.log protocols $ V.fromList $ map toThrift (toList msgs)
         putStrLn $ "Got response: " ++ show resp


  -- Now launch some queries!
  -- TODO: maybe convert this test suite to detailed-1.0. Then we can print useful
  -- stuff and return a list of Test where each Test tests some query
  -- Maybe Quickcheck + being able to test aggregations ourselves would be cool
  return []

main = do
  tests
  return ()
