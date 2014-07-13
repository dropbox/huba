{-# LANGUAGE OverloadedStrings, InstanceSigs #-}

module LeafNode (newLeafNodeHandler) where

import Huba_Types

import qualified LeafNodeService
import LeafNodeService_Iface

import Thrift()
import Thrift.Protocol.Binary()
import Thrift.Transport()
import Thrift.Server

import Data.Vector (Vector())
import Control.Applicative ((<$>))


data LeafNodeHandler = LeafNodeHandler

newLeafNodeHandler :: IO LeafNodeHandler
newLeafNodeHandler = return LeafNodeHandler

instance LeafNodeService_Iface LeafNodeHandler where
  log :: LeafNodeHandler -> Maybe (Vector LogMessage) -> IO LogResponse
  log _ (Just messages) = do
    putStrLn $ "Got messages: " ++ show messages
    putStrLn "log()"
    return $ LogResponse (Just 0) Nothing
