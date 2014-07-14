{-# LANGUAGE OverloadedStrings, InstanceSigs #-}

module Ingestor.Server (newIngestorHandler) where

import Shared.Thrift.Interface
import Data.Vector (Vector())


data IngestorHandler = IngestorHandler

newIngestorHandler :: IO IngestorHandler
newIngestorHandler = return IngestorHandler

instance IngestorService IngestorHandler where
  logIngest :: IngestorHandler -> Vector LogMessage -> IO LogResponse
  logIngest _ messages = do
    putStrLn $ "Got messages: " ++ show messages
    putStrLn "log()"
    return $ LogResponse 0 "OK"
