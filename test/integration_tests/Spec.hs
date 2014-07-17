-- module Spec (tests) where
module Main where

import Distribution.TestSuite
import Launch (runServer)
import Testing.Util (genRandomLogMessage, waitForServer)

import Shared.Thrift.Interface
import Shared.Thrift.ClientInterface (sendIngestorLog, Server(..))

import Control.Concurrent.Async (async, waitAny, mapConcurrently)
import Control.Applicative ( (<$>) )
import Network
import Data.Foldable (toList)
import Data.Vector as V (fromList)
import Control.Monad (replicateM_, replicateM, forM, liftM)

import System.Timeout (timeout)

import qualified Data.Random.Source.DevRandom as R
import qualified Data.Random as R

import Data.Maybe (isJust)

import System.Log.Logger

tests :: IO [Test]
tests = do
  noticeM "Integration tests" "Running integration tests..."

  -- Start a number of servers
  let n = 8
      basePorts = take n $ iterate (+100) 8000
      portTuples = [(x, x+1, x+2, x+3) | x <- basePorts]
      allPorts = concat [[x, x+1, x+2, x+3] | x <- basePorts]

  processes <- concat <$> forM portTuples runServer

  -- Make sure all services respond to ping within some timeout
  pingResponses <- mapConcurrently
                   (\x -> timeout (5 * 10^6) (waitForServer $ Server "localhost" x))
                   allPorts
  if (length . filter isJust) pingResponses < length allPorts then
      noticeM "Integration tests" "FAILURE: Not all servers responded to ping" else
      noticeM "Integration tests" "SUCCESS on ping test"


  -- Send a bunch of random LogMessages into the system
  let server = Server "localhost" 8000
      numBatches = 10
      batchSize = 10

  replicateM_ numBatches $ do
         -- Get batchSize random messages
         msgs <- replicateM batchSize $ R.runRVar genRandomLogMessage R.DevURandom
         resp <- sendIngestorLog server $ V.fromList msgs
         putStrLn $ "Got response: " ++ show resp

  -- Uncommenting this line lets us wait on all the processes
  -- _ <- waitAny $ concat processes

  -- Now launch some queries!
  -- TODO: maybe convert this test suite to detailed-1.0. Then we can print useful
  -- stuff and return a list of Test where each Test tests some query
  -- Maybe Quickcheck + being able to test aggregations ourselves would be cool
  return []

main = do
  tests
  return ()
