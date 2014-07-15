{-# LANGUAGE OverloadedStrings, InstanceSigs #-}

module Aggregator.Server where

import Shared.Thrift.Interface

import Control.Applicative ((<$>))
import LeafNode.Datastore (LeafStore, ingestBatch)

import Control.Concurrent.MVar(MVar(), newMVar, modifyMVar_, readMVar)

-- data LeafNodeHandler = LeafNodeHandler (MVar LeafStore)

-- newLeafNodeHandler :: IO LeafNodeHandler
-- newLeafNodeHandler = LeafNodeHandler <$> newMVar []

-- instance LeafNodeService LeafNodeHandler where
--   logLeaf :: LeafNodeHandler -> LogBatch -> IO LogResponse
--   logLeaf (LeafNodeHandler logs) batch = do
--     putStrLn $ "LeafNode received message: " ++ show batch
--     modifyMVar_ logs $ return . (`ingestBatch` batch)

--     logContents <- readMVar logs
--     putStrLn $ "Current contents" ++ show logContents
--     return $ LogResponse 0 "OK"
