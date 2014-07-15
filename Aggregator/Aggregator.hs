module Aggregator where

import System.Random (mkStdGen)
import System.Random.Shuffle (shuffle')

fanoutFactor :: Int
fanoutFactor = 5

-- XXX
data Query = Query
data QueryResponse = QueryResponse
data Server = Server
type Index = Int
type Seed = Int



-- TODO: this
fanout :: Seed -> Index -> [Server] -> [Server]
fanout seed myIndex = id -- chooseChildren . shuffleList
    where chooseChildren :: [Server] -> [Server]
          chooseChildren = undefined
          shuffleList :: [Server] -> [Server]
          shuffleList ss = shuffle' ss (length ss) (mkStdGen seed)

aggregate :: [QueryResponse] -> QueryResponse
aggregate = undefined -- TODO: this


rootQueryTransform :: Query -> Query
rootQueryTransform = id -- TODO: this


rootResponseTransform :: QueryResponse -> QueryResponse
rootResponseTransform = id -- TODO: this
