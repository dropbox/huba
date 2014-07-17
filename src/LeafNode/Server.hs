{-# LANGUAGE OverloadedStrings, InstanceSigs #-}

module LeafNode.Server (newLeafNodeHandler) where

import Shared.Thrift.Types
import Shared.Thrift.Interface

import Control.Applicative ((<$>), (<*>))
import LeafNode.Datastore (LeafStore, ingestBatch, query)

import Control.Concurrent.MVar(MVar(), newMVar, modifyMVar_, readMVar)

import System.Log.Logger

data LeafNodeHandler = LeafNodeHandler (MVar LeafStore)

newLeafNodeHandler :: IO LeafNodeHandler
newLeafNodeHandler = LeafNodeHandler <$> newMVar []

instance CommonService LeafNodeHandler

instance LeafNodeService LeafNodeHandler where
  logLeaf :: LeafNodeHandler -> LogBatch -> IO LogResponse
  logLeaf (LeafNodeHandler logs) batch = do
    infoM "LeafNode" $ "Received log batch."
    debugM "LeafNode" $ show batch
    modifyMVar_ logs $ return . (`ingestBatch` batch)

    logContents <- readMVar logs
    debugM "LeafNode" $ "Current state:" ++ show logContents
    return $ LogResponse 0 "OK"

  queryLeaf :: LeafNodeHandler -> Query -> IO QueryResponse
  queryLeaf (LeafNodeHandler logs) q = query <$> readMVar logs <*> return q

