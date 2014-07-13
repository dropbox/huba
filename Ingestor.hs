{-# LANGUAGE OverloadedStrings, InstanceSigs #-}

module Ingestor (newIngestorHandler) where

import Huba_Types

import qualified IngestorService
import IngestorService_Iface

import Thrift()
import Thrift.Protocol.Binary()
import Thrift.Transport()
import Thrift.Server

import Data.Vector (Vector())
import Control.Applicative ((<$>))


data IngestorHandler = IngestorHandler

newIngestorHandler :: IO IngestorHandler
newIngestorHandler = return IngestorHandler

instance IngestorService_Iface IngestorHandler where
  log :: IngestorHandler -> Maybe (Vector LogMessage) -> IO LogResponse
  log _ (Just messages) = do
    putStrLn $ "Got messages: " ++ show messages
    putStrLn "log()"
    return $ LogResponse (Just 0) Nothing
