{-# LANGUAGE OverloadedStrings, InstanceSigs #-}

module Aggregator.Server where

import Shared.Thrift.Types
import Shared.Thrift.Interface
import Shared.Thrift.ClientInterface
import Shared.Config (ServerList, getIntermediateAggregators)

import Aggregator.Aggregator

import Control.Monad
import Control.Applicative ((<$>))
import Control.Concurrent.Async
import System.Random.Shuffle (shuffleM)

import Data.Maybe (catMaybes)
import qualified Data.Vector as V

import System.Timeout (timeout)
import System.Log.Logger

timeoutInterval :: Int
timeoutInterval = 30 * 10^(6 :: Int) -- microseconds

data RootAggregator = RootAggregator ServerList -- intermediate aggregators
newRootAggregator :: IO RootAggregator
newRootAggregator = RootAggregator <$> getIntermediateAggregators

instance CommonService RootAggregator

instance AggregatorService RootAggregator where
  queryAggregator :: RootAggregator -> Query -> IO QueryResponse
  queryAggregator (RootAggregator serverList) query = do
    -- TODO: guarantee that this gets sent to the local intermediate aggregator
    servers <- getAggregatorIDs >>= shuffleM
    infoM "RootAggregator" $ "Received query.  Sending to " ++ show (getAggregator serverList $ head servers)
    debugM "RootAggregator" $ show query
    response <- sendInternalAggregatorQuery (getAggregator serverList $ head servers) (rootQueryTransform query) (V.fromList servers)
    return $ rootResponseTransform response

data IntermediateAggregator = IntermediateAggregator [Server] ServerList -- local leaf nodes, intermediate aggregators
newIntermediateAggregator :: [Server] -> IO IntermediateAggregator
newIntermediateAggregator leaves = IntermediateAggregator leaves <$> getIntermediateAggregators

instance CommonService IntermediateAggregator

instance InternalAggregatorService IntermediateAggregator where
  queryInternalAggregator :: IntermediateAggregator -> Query -> V.Vector ServerID -> IO QueryResponse
  queryInternalAggregator (IntermediateAggregator leaves serverList) query serverIDs
    -- no more fanning out needed - query our local LeafNodes
    | V.length serverIDs == 1 = do
      infoM "LeafAggregator" $ "Received query.  Sending to local leaf nodes: " ++ show (leaves)
      responses <- flip mapConcurrently leaves $ \leaf ->
        join <$> (timeout timeoutInterval $ sendLeafQuery leaf query)

      return $ aggregate query $ catMaybes responses

    -- fan out the query to other IntermediateAggregators
    | otherwise = do
      infoM "IntermediateAggregator" $ "Received query.  Fanning out to " ++ show (map (getAggregator serverList . V.head) $ fanout serverIDs)
      responses <- flip mapConcurrently (fanout serverIDs) $ \ss ->
        join <$> (timeout timeoutInterval $ sendInternalAggregatorQuery (getAggregator serverList $ V.head ss) query ss)

      return $ aggregate query $ catMaybes responses



getAggregatorIDs :: IO [ServerID]
getAggregatorIDs = do
    ags <- getIntermediateAggregators
    return [0 .. fromIntegral $ length ags - 1]

getAggregator :: ServerList -> ServerID -> Server
getAggregator list sid = list !! fromIntegral sid
