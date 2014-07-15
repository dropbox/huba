{-# LANGUAGE OverloadedStrings #-}

module LeafNode.Datastore (LeafStore(), ingestBatch) where

import Shared.Thrift.Types
import Shared.Thrift.Interface as I
import qualified Data.Vector as V
import Data.List (insert)

import Control.Lens
import Data.Maybe (fromMaybe)

type LeafStore = [LogMessage]


ingestBatch :: LeafStore -> LogBatch -> LeafStore
ingestBatch = V.foldl' (flip insert)

getMessagesInTimeRange :: LeafStore -> Timestamp -> Timestamp -> [LogMessage]
getMessagesInTimeRange store timeStart timeEnd = filter
                                                 (\x -> ((x ^. lmTimestamp) >= timeStart) && ((x ^. lmTimestamp) <= timeEnd))
                                                 store

makeCondition :: Condition -> (LogMessage -> Bool)
makeCondition (Condition col comp val) = undefined
  where compFn I.EQ        = (==) -- applies to all types
        compFn I.NEQ       = (/=)

        compFn I.GT        = (>)
        compFn I.LT        = (<)
        compFn I.GTE       = (>=)
        compFn I.LTE       = (<=)

        compFn I.REGEXP_EQ = undefined

getColFromMessage :: ColumnName -> LogMessage -> Maybe ColumnValue
getColFromMessage name msg = msg ^. lmColumns . at name

-- makeFn :: ColumnName -> ColumnValue -> (a -> a -> Bool) -> LogMessage -> Bool
-- makeFn col val compFn msg = fromMaybe False $ do
--   colValue <- getColFromMessage col msg
--   return colValue `compFn` val


makeConditions :: [Condition] -> (LogMessage -> Bool)
makeConditions conditions message = all (($ message) . makeCondition) conditions

-- answerQuery :: LeafStore -> Query -> QueryResponse
-- answerQuery store q = QueryResponse 0 (Just "asdf") Nothing
--     where queriesInTimeRange = getMessagesInTimeRange store (qTimeStart q) (qTimeEnd q)
--           filteredQueries = filter (makeCondition (qConditions q)) queriesInTimeRange




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
