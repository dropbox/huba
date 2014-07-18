{-# LANGUAGE OverloadedStrings, OverloadedLists #-}
module Testing.Util where

import qualified Huba_Types as T

import Shared.Thrift.Types
import Shared.Thrift.Interface
import Shared.Thrift.ClientInterface

import qualified Data.Random as R
import qualified Data.Random.Extras as RE
import qualified Data.HashMap.Lazy as Map

import qualified Data.Text.Lazy as L

import Thrift
import Thrift.Protocol.Binary
import Thrift.Transport.Handle
import qualified CommonService_Client as CommonClient

import Control.Concurrent.Async (async)
import Control.Exception (catch, SomeException)
import Control.Applicative ((<$>))

import Data.Maybe (fromMaybe)

import System.Log.Logger

genRandomLogMessage = do
  ts <- R.uniform 0 100000
  table <- RE.choice ["tableA", "tableB", "tableC"]

  string1 <- RE.sample 1 ['a'..'z']
  int1 <- R.uniform 0 100

  -- numStringsInVector <- R.uniform 0 10
  -- TODO: make a vector with numStringsInVector random strings and add it to the message with some probability
  -- TODO: make there be a chance of adding a random set of strings
  return $ LogMessage ts table $ Map.fromList [("string1", StringValue $ L.pack string1),
                                               ("int1", IntValue int1)]

waitForServer :: Server -> IO PingResponse
waitForServer server = do
    resp <- catch (sendPing server) (\e -> do
                                       let err = show (e :: SomeException)
                                       noticeM "waitForServer" "Exception on ping!"
                                       Just <$> waitForServer server)

    maybe (waitForServer server) return resp
