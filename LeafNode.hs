{-# LANGUAGE OverloadedStrings, InstanceSigs #-}

module LeafNode (newLeafNodeHandler) where

-- import qualified Huba_Types as T

import ThriftInterface

import qualified LeafNodeService ()

import Thrift ()
import Thrift.Protocol.Binary ()
import Thrift.Transport ()
-- import Thrift.Server

import Data.Vector (Vector(), foldl')
import Control.Applicative ((<$>))
import Data.List (insert)

import Control.Concurrent.MVar(MVar(), newMVar, modifyMVar_, readMVar)

data LeafNodeHandler = LeafNodeHandler (MVar [LogMessage])

newLeafNodeHandler :: IO LeafNodeHandler
newLeafNodeHandler = LeafNodeHandler <$> newMVar []

instance LeafNodeService LeafNodeHandler where
  log :: LeafNodeHandler -> Vector LogMessage -> IO LogResponse
  log (LeafNodeHandler logs) message = do
    putStrLn $ "LeafNode received message: " ++ show message
    modifyMVar_ logs $ return . \ls -> foldl' (flip insert) ls message

    logContents <- readMVar logs
    putStrLn $ "Current contents" ++ show logContents
    return $ LogResponse 0 "OK"
