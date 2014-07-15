module LeafNode.Datastore (LeafStore(), ingestBatch) where

import Shared.Thrift.Interface
import Data.Vector (foldl')
import Data.List (insert)

type LeafStore = [LogMessage]

ingestBatch :: LeafStore -> LogBatch -> LeafStore
ingestBatch = foldl' (flip insert)



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
