{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

module LeafNode.Datastore (LeafStore(), ingestBatch, query) where

import Shared.Thrift.Types as T
import Shared.Thrift.Interface
import Shared.Comparison
import Shared.Query (orderRows)
import LeafNode.Aggregations

import qualified Data.Vector as V
import Data.List (insert, sortBy)

import Control.Monad (liftM)
import Control.Lens
import Control.Lens.TH
import Data.Maybe (fromMaybe, isNothing, fromJust)

import Data.Int (Int32)
import Data.Ord (comparing)

import qualified Data.HashMap.Strict as H

type LeafStore = [LogMessage]

{- Store a batch of messages in a LeafStore -}
ingestBatch :: LeafStore -> LogBatch -> LeafStore
ingestBatch = V.foldl' (flip insert)

{- Answer a query -}
query :: LeafStore -> Query -> QueryResponse
query store q = QueryResponse 0 Nothing (Just $ V.fromList responseRows)
    where
      -- Get the rows in the desired time range
      rowsInTimeRange = getMessagesInTimeRange store (q ^. qTimeStart) (q ^. qTimeEnd)

      -- TODO: make this and filterFn nicer
      makeConditions conds message = all (($ message) . makeCondition) conds

      filterFn :: [LogMessage] -> [LogMessage]
      filterFn = let conditionFn = case q ^. qConditions of
                                     Nothing -> const True
                                     Just conditions -> makeConditions $ V.toList conditions in
                 -- Filter by all the conditions plus the table match
                 filter (\x -> conditionFn x && (q ^. qTable == x ^. lmTable))
                 -- TODO: let's make conditions a [Condition] instead of a Vector Condition (?)

      processFn = processMessages q

      responseRows = (take (q ^. qLimit) . orderRows q . processFn . filterFn) rowsInTimeRange
      -- TODO: make sure the sort interacts with the limit/processing efficiently here.
      -- Taking k things from a sorted list of length n shouldn't take n log n.
      -- You can do faster with a quicksort that ignores the part of the list it doesn't
      -- need to sort.
      -- Alternatively you can do n log k by continually doing sorted insert (and delete)
      -- into a binary tree of size k

----------------------------------------------------------------------------------------------------------

processMessages :: Query -> [LogMessage] -> [Row] -- TODO: let's rename Row to ResponseRow
processMessages q@(Query ce _ _ _ _ groupBy _ _) msgs =
    if isNothing groupBy then
        if isSimpleQuery q then
            map (extractColumnsAsRow $ fmap (^. ceColumn) ce) msgs
        else
            [processSingleGroupBy q msgs]
    else map (processSingleGroupBy q) (groupLogMessages (fromJust groupBy) msgs)


getMessagesInTimeRange :: LeafStore -> Timestamp -> Timestamp -> [LogMessage]
getMessagesInTimeRange store timeStart timeEnd = filter
                                                 (\x -> ((x ^. lmTimestamp) >= timeStart) && ((x ^. lmTimestamp) <= timeEnd))
                                                 store

extractColumnValues :: V.Vector ColumnName -> LogMessage -> V.Vector (Maybe ColumnValue)
extractColumnValues cols (LogMessage _ _ columns) = fmap (`H.lookup` columns) cols

extractColumnsAsRow :: V.Vector ColumnName -> LogMessage -> Row
extractColumnsAsRow cols msg = Row $ fmap (fromMaybe RNull . liftM columnValueToResponseValue)
                                          (extractColumnValues cols msg)

isSimpleQuery :: Query -> Bool
isSimpleQuery (Query columnExpressions _ _ _ _ groupBy _ _) = isNothing groupBy &&
                                                              V.all (== CONSTANT)  (fmap (^. ceAggregationFunction) columnExpressions)
{-
  Given a query and a collection of log messages belonging to a single groupBy
  value, generate the single Row for the groupBy value.
 -}
processSingleGroupBy :: Query -> [LogMessage] -> Row
processSingleGroupBy (Query columnExpressions _ _ _ _ groupBy _ _) rows =
    Row $ foldl aggFn initialValues (map projectionFn rows) where
        columnNames = fmap (^. ceColumn) columnExpressions
        aggregationFunctions = fmap (^. ceAggregationFunction) columnExpressions

        projectionFn = extractColumnValues columnNames

        aggFunctionsAndInitialValues = V.map aggFunctionAndInitialValue aggregationFunctions

        aggFns = V.map fst aggFunctionsAndInitialValues :: V.Vector FoldFunction
        initialValues = V.map snd aggFunctionsAndInitialValues :: V.Vector ResponseValue

        aggFn = V.zipWith3 ($) aggFns :: V.Vector ResponseValue -> V.Vector (Maybe ColumnValue) -> V.Vector ResponseValue

groupLogMessages :: V.Vector ColumnName -> [LogMessage] -> [[LogMessage]]
groupLogMessages = undefined
