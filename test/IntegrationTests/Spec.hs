{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

module Main where

import Distribution.TestSuite
import Launch (runServer)
import Testing.Util (genRandomLogMessage, waitForServer)

import Shared.Thrift.Interface
import Shared.Thrift.ClientInterface (sendIngestorLog, Server(..))
import Shared.Thrift.Types as T

import Control.Concurrent.Async (async, waitAny, mapConcurrently, Async, cancel)
import Control.Applicative ( (<$>) )
import Network
import Data.Foldable (toList)
import Data.Vector as V (fromList)
import Control.Monad (replicateM_, replicateM, forM, liftM, mapM_)

import System.Timeout (timeout)

import Data.Maybe (isJust, isNothing)

import System.Log.Logger

import Common.TestData (makeSpec)
import Test.Hspec
import Shared.Thrift.ClientInterface (sendRootQuery)

import GHC.Conc.IO (threadDelay)

runTestWithServers :: IO () -> IO ()
runTestWithServers specAction = do
  noticeM "runTestWithServers" "Starting servers"

  -- Start a number of servers
  let n = 8
      basePorts = take n $ iterate (+100) 8000
      portTuples = [(x, x+1, x+2, x+3) | x <- basePorts]
      allPorts = concat [[x, x+1, x+2, x+3] | x <- basePorts]

  threads <- concat <$> forM portTuples runServer

  -- Make sure all services respond to ping within some timeout
  pingResponses <- timeout (5 * 10^6) $ mapConcurrently (\x -> waitForServer $ Server "localhost" x) allPorts

  if isNothing pingResponses then do
      noticeM "Integration tests" "FAILURE: Not all servers responded to ping"
      hspec $ describe "Pinging" $
        it "fails" $ True `shouldBe` False
  else do
      noticeM "Integration tests" "SUCCESS on ping test"

      ------------------
      specAction
      ------------------

      noticeM "runTestWithServers" "Stopping servers"
      mapM_ cancel threads

      return ()


testQueryResponse :: LogBatch -> Query -> QueryResponse -> Expectation
testQueryResponse messages query queryResponse = do
  ingestResponse <- sendIngestorLog (Server "localhost" 8000) messages
  threadDelay (1 * 10^5) -- Need a little time for the data to reach the leaf nodes & get indexed
  noticeM "testQueryResponse" $ "Done waiting! Sending query: " ++ show query
  queryResp <- sendRootQuery (Server "localhost" 8001) query
  queryResp `shouldBe` Just queryResponse


nonTransformedMessagesSpec = makeSpec testQueryResponse

tests =
   around runTestWithServers $ describe "Simple aggregations"
       nonTransformedMessagesSpec






  -- Uncommenting this line lets us wait on all the threads
  -- _ <- waitAny $ concat threads

main = hspec tests
