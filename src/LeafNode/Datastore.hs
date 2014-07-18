{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

module LeafNode.Datastore (LeafStore(), ingestBatch, query) where

import Shared.Thrift.Types as T
import Shared.Thrift.Interface
import Shared.Comparison
import Shared.Query (orderRows)
import Shared.Aggregation (aggregateRows)

import qualified Data.Vector as V
import Data.List (insert, sortBy, groupBy)

import Control.Monad (liftM)
import Control.Lens
import Control.Lens.TH
import Data.Maybe (fromMaybe, isNothing, fromJust, catMaybes)

import Data.Int (Int32)
import Data.Ord (comparing)

import Data.Function (on)

import qualified Data.HashMap.Lazy as Map
import qualified Data.HashMap.Strict as H


type LeafStore = [LogMessage]

{- Store a batch of messages in a LeafStore -}
ingestBatch :: LeafStore -> LogBatch -> LeafStore
ingestBatch = V.foldl' (flip insert)

{- Answer a query -}
query :: LeafStore -> Query -> QueryResponse
query store q = QueryResponse 0 Nothing (Just $ V.fromList responseRows)
    where
      rowsInTimeRange = getMessagesInTimeRange store (q ^. qTimeStart) (q ^. qTimeEnd)

      -- TODO: make this and filterFn nicer
      makeConditions conds message = all (($ message) . makeCondition) conds

      filterFn :: [LogMessage] -> [LogMessage]
      filterFn = let conditionFn = case q ^. qConditions of
                                     Nothing -> const True
                                     Just conditions -> makeConditions $ V.toList conditions in
                 filter (\x -> conditionFn x && (q ^. qTable == x ^. lmTable))

      projectFn :: [LogMessage] -> [Row]
      projectFn = liftM $ extractColumnsAsRow (q ^. qColumnExpressions)

      responseRows = (take (q ^. qLimit) . orderRows q . aggregateRows q . projectFn . filterFn) rowsInTimeRange
      -- TODO: make sure the sort interacts with the limit/processing efficiently here.

----------------------------------------------------------------------------------------------------------

{-
  Given a LeafStore and two Timestamps, return all messages taking place between the Timestamps
 -}
getMessagesInTimeRange :: LeafStore -> Timestamp -> Timestamp -> [LogMessage]
getMessagesInTimeRange store timeStart timeEnd = filter
                                                 (\x -> ((x ^. lmTimestamp) >= timeStart) && ((x ^. lmTimestamp) <= timeEnd))
                                                 store


extractColumnsAsRow :: V.Vector ColumnExpression -> LogMessage -> Row
extractColumnsAsRow columnExpressions (LogMessage _ _ columns) = fmap (projectColumnExpression columns) columnExpressions

projectColumnExpression ::  Map.HashMap ColumnName ColumnValue -> ColumnExpression -> ResponseValue
projectColumnExpression columns (ColumnExpression name fn) = fromMaybe RNull $ liftM project $ H.lookup name columns where
    project v = if fn == T.COUNT then RIntValue 1 else columnValueToResponseValue v

    columnValueToResponseValue :: ColumnValue -> ResponseValue
    columnValueToResponseValue (StringValue s) = RStringValue s
    columnValueToResponseValue (IntValue i) = RIntValue i
    columnValueToResponseValue (StringSet s) = RStringSet s
    columnValueToResponseValue (StringVector s) = RStringVector s
