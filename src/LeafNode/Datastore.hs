{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

module LeafNode.Datastore (LeafStore(), ingestBatch, query) where

import Shared.Thrift.Types as T
import Shared.Thrift.Interface
import Shared.Comparison

import qualified Data.Vector as V
import Data.List (insert, sortBy)

import Control.Lens
import Control.Lens.TH
import Data.Maybe (fromMaybe)

import Data.Int (Int32)
import Data.Ord (comparing)
import Data.Maybe

import qualified Data.HashMap.Strict as H

type LeafStore = [LogMessage]


ingestBatch :: LeafStore -> LogBatch -> LeafStore
ingestBatch = V.foldl' (flip insert)

getMessagesInTimeRange :: LeafStore -> Timestamp -> Timestamp -> [LogMessage]
getMessagesInTimeRange store timeStart timeEnd = filter
                                                 (\x -> ((x ^. lmTimestamp) >= timeStart) && ((x ^. lmTimestamp) <= timeEnd))
                                                 store


getColFromMessage :: ColumnName -> LogMessage -> Maybe ColumnValue
getColFromMessage name msg = msg ^. lmColumns . at name


makeCondition :: Condition -> (LogMessage -> Bool)
makeCondition (Condition col comp val) msg = let compFn = case comp of T.EQ -> columnValueEQ
                                                                       T.NEQ -> columnValueNEQ

                                                                       T.GT -> columnValueGT
                                                                       T.LT -> columnValueLT
                                                                       T.GTE -> columnValueGTE
                                                                       T.LTE -> columnValueLTE

                                                                       T.REGEXP_EQ -> columnValueREGEXPEQ in

  fromMaybe False $ do
    colValue <- getColFromMessage col msg
    return (colValue `compFn` val)




makeConditions :: [Condition] -> (LogMessage -> Bool)
makeConditions conditions message = all (($ message) . makeCondition) conditions

-- selectAggregator
-- aggAggregator

-- data Aggregator = Aggregator (State a)

-- aggregator :: LogMessage -> a -> [Row]


extractColumns :: V.Vector ColumnName -> LogMessage -> Row
extractColumns cols (LogMessage _ _ columns) = Row (fmap getColumnValue cols) where
    getColumnValue :: ColumnName -> ResponseValue
    getColumnValue col = fromMaybe RNull $ do
                           columnValue <- H.lookup col columns
                           return (columnValueToResponseValue columnValue)

isSimpleQuery :: Query -> Bool
isSimpleQuery (Query columnExpressions _ _ _ _ groupBy _ _) = isNothing groupBy &&
                                                              V.all (== CONSTANT)  (fmap (^. ceAggregationFunction) columnExpressions)

processRows :: Query -> [LogMessage] -> [Row] -- TODO: let's rename Row to ResponseRow
processRows q@(Query columnExpressions _ _ _ _ groupBy _ _) rows =
    if isSimpleQuery q then let columnNames = fmap (^. ceColumn) columnExpressions in
                            map (extractColumns columnNames) rows
    else []

    -- do
    --   return ()

    -- where groups = H.empty :: HashMap [ColumnName] Aggregator
    -- For each row, select the right aggregator (based on the groupBy) and run it on the row
    -- The aggregator emits a new state

query :: LeafStore -> Query -> QueryResponse
query store q = QueryResponse 0 Nothing (Just $ V.fromList responseRows)
    where
      -- Get the rows in the desired time range
      rowsInTimeRange = getMessagesInTimeRange store (q ^. qTimeStart) (q ^. qTimeEnd)

      filterFn :: [LogMessage] -> [LogMessage]
      filterFn = let conditionFn = case q ^. qConditions of
                                     Nothing -> const True
                                     Just conditions -> makeConditions $ V.toList conditions in
                 -- Filter by all the conditions plus the table match
                 filter (\x -> conditionFn x && (q ^. qTable == x ^. lmTable))
                 -- TODO: let's make conditions a [Condition] instead of a Vector Condition, yes?

      processFn = processRows q

      sortWith :: Maybe Int -> [Row] -> [Row]
      sortWith Nothing = id
      sortWith (Just index) = sortBy (comparing ((V.! index) . view rValues))

      limitBy :: Maybe Int -> [Row] -> [Row]
      limitBy Nothing = id
      limitBy (Just n) = take n

      responseRows = (limitBy (q ^. qLimit) . sortWith (q ^. qOrderBy) . processFn . filterFn) rowsInTimeRange
      -- TODO: make sure the sort interacts with the limit/processing efficiently here.
      -- Taking k things from a sorted list of length n shouldn't take n log n.
      -- You can do faster with a quicksort that ignores the part of the list it doesn't
      -- need to sort.
      -- Alternatively you can do n log k by continually doing sorted insert (and delete)
      -- into a binary tree of size k




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
