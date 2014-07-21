module LeafNode.Datastore (LeafStore(), ingestBatch, query, groupBy) where

import Shared.Thrift.Types
import Shared.Comparison (makeCondition)
import Shared.Row (project)
import Shared.Aggregation (groupBy)

import Control.Lens
import Data.List (sortBy, insert)
import Data.Ord (comparing)
import qualified Data.Vector as V
import qualified Data.HashMap.Lazy as M

type LeafStore = [LogMessage]

{- Store a batch of messages in a LeafStore -}
ingestBatch :: LeafStore -> LogBatch -> LeafStore
ingestBatch = V.foldl' (flip insert)

{- Answer a query -}
query :: LeafStore -> Query -> QueryResponse
query ms (Query cols table startTime endTime conditions group order limit)
  = ms & filterTable table
       & filterTime startTime endTime
       & filterConditions conditions
       & pluckColumns cols
       & groupBy group cols
       & orderBy order
       & take limit
       & V.fromList & Just & QueryResponse 0 Nothing
  where filterTable t = filter (^. lmTable . to (== t))
        filterTime start end = filter (^. lmTimestamp . to (\t -> t >= start && t <= end))
        filterConditions Nothing = id
        filterConditions (Just cs) = let conditionFns = V.map makeCondition cs in
                                       filter (\msg -> V.all ($ msg) conditionFns)
        pluckColumns cs = map (project cs)
        orderBy Nothing = id
        orderBy (Just o) = sortBy (comparing (V.! o))
