module Shared.Config (
  ServerList,
  getIntermediateAggregators,
  getLeafNodes
) where

import Shared.Thrift.ClientInterface
import Control.Applicative ((<$>))


type ServerList = [Server]

getIntermediateAggregators :: IO ServerList
getIntermediateAggregators = readServerConfig "config/IntermediateAggregator.conf"

getLeafNodes :: IO ServerList
getLeafNodes = readServerConfig "config/LeafNode.conf"


readServerConfig :: FilePath -> IO ServerList
readServerConfig file = map readLine . map words . lines <$> readFile file
  where readLine [name, port] = Server name (fromInteger $ read port)
        readLine _            = error $ "Config file " ++ file ++ " is invalid!"
