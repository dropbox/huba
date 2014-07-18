{-# LANGUAGE OverloadedStrings, InstanceSigs #-}

module Ingestor.Server (newIngestorHandler) where

import Shared.Thrift.Types
import Shared.Thrift.Interface
import Shared.Thrift.ClientInterface

import Shared.Config (ServerList, getLeafNodes)
import System.Random (randomRIO)

import Control.Applicative ((<$>))
import Data.Maybe (fromJust)

import System.Log.Logger

data IngestorHandler = IngestorHandler ServerList -- leaf nodes

newIngestorHandler :: IO IngestorHandler
newIngestorHandler = IngestorHandler <$> getLeafNodes

instance CommonService IngestorHandler

-- TODO: choose based on memory constraints
instance IngestorService IngestorHandler where
  logIngest :: IngestorHandler -> LogBatch -> IO LogResponse
  logIngest (IngestorHandler leaves) messages = do
    leaf <- pick leaves

    infoM "Ingestor" $ "Received log batch. Sending to " ++ show leaf
    debugM "Ingestor" $ show messages

    fromJust <$> sendLeafLog leaf messages -- XXX fromJust

pick :: [a] -> IO a
pick xs = randomRIO (0, length xs - 1) >>= return . (xs !!)
