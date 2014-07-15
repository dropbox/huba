module LeafNode.Datastore (LeafStore(), ingestBatch) where

import Shared.Thrift.Interface
import qualified Data.Vector as V
import Data.List (insert)

import Control.Applicative (liftA2)

import Data.Int (Int64)

type LeafStore = [LogMessage]


ingestBatch :: LeafStore -> LogBatch -> LeafStore
ingestBatch = V.foldl' (flip insert)

getMessagesInTimeRange :: LeafStore -> Int64 -> Int64 -> [LogMessage]
getMessagesInTimeRange store timeStart timeEnd = filter
                                                 (\x -> (lmTimestamp x >= timeStart) && (lmTimestamp x <= timeEnd))
                                                 store

makeCondition = undefined

answerQuery :: LeafStore -> Query -> QueryResponse
answerQuery store q = QueryResponse "asdf"
    where queriesInTimeRange = getMessagesInTimeRange store (timeStart q) (timeEnd q)
          filteredQueries = filter (makeCondition (conditions q)) queriesInTimeRange




-------------------------

-- type ConName = String

-- data AggFn = Constant | Sum | Count
-- data Aggregator = Aggregator ColName AggFn


-- project :: Row -> Aggregator -> Aggregate
-- project row (Aggregator colName Sum) = Aggregate $ getCol colName row
-- project row (Aggregator colName Count) = Aggregate $ 1
-- project row (Aggregator colName Constant) = Aggregate $ getCol colName row


-- aggAppend :: AggFn -> Aggregate -> Aggregate -> Aggregate
-- aggAppend Sum (Aggregate x) (Aggregate y) = Aggregate (x + y)
-- aggAppend Count (Aggregate x) (Aggregate y) = Aggregate (x + y)
-- aggAppend Constant a1 _ = a1
