{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

module LeafNode.Datastore (LeafStore(), ingestBatch, query) where

import Shared.Thrift.Types as T
import Shared.Thrift.Interface
import Shared.Comparison
import Shared.Query (orderRows)
import LeafNode.Aggregations

import qualified Data.Vector as V
import Data.List (insert, sortBy, groupBy)

import Control.Monad (liftM)
import Control.Lens
import Control.Lens.TH
import Data.Maybe (fromMaybe, isNothing, fromJust, catMaybes)

import Data.Int (Int32)
import Data.Ord (comparing)

import Data.Function (on)

import qualified Data.HashMap.Strict as H

import qualified Data.Map.Lazy as L

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
      projectFn = liftM $ extractColumnsAsRow (fmap _ceColumn (q ^. qColumnExpressions))

      responseRows = (take (q ^. qLimit) . orderRows q . processGroupBys q . projectFn . filterFn) rowsInTimeRange
      -- TODO: make sure the sort interacts with the limit/processing efficiently here.

----------------------------------------------------------------------------------------------------------

processGroupBys :: Query -> [Row] -> [Row] -- TODO: let's rename Row to ResponseRow
processGroupBys q@(Query ce _ _ _ _ groupBy _ _) rows =
    if isNothing groupBy then
        if isSimpleQuery q then
            rows
        else
            [processSingleGroupBy q rows]
    else map (processSingleGroupBy q) (groupRowsUsingIndices groupByIndices rows) where -- TODO: cleanup these lines
        groupByIndices = catMaybes [V.findIndex (\x -> x ^. ceColumn == groupByName) (q ^. qColumnExpressions) | groupByName <- V.toList $ fromJust groupBy]

extractColumnsAsRow :: V.Vector ColumnName -> LogMessage -> Row
extractColumnsAsRow cols (LogMessage _ _ columns) = fmap (fromMaybe RNull . liftM columnValueToResponseValue)
                                                         (fmap (`H.lookup` columns) cols)

isSimpleQuery :: Query -> Bool
isSimpleQuery (Query columnExpressions _ _ _ _ groupBy _ _) = isNothing groupBy &&
                                                              V.all (== CONSTANT)  (fmap (^. ceAggregationFunction) columnExpressions)
{-
  Given a query and a collection of log messages belonging to a single groupBy
  value, generate the single Row for the groupBy value.
 -}
processSingleGroupBy :: Query -> [Row] -> Row
processSingleGroupBy (Query columnExpressions _ _ _ _ _ _ _) = foldl aggFn initialValues where
    aggFunctionsAndInitialValues = V.map aggFunctionAndInitialValue $ fmap (^. ceAggregationFunction) columnExpressions

    aggFns = V.map fst aggFunctionsAndInitialValues :: V.Vector FoldFunction
    initialValues = V.map snd aggFunctionsAndInitialValues :: V.Vector ResponseValue

    aggFn = V.zipWith3 ($) aggFns :: Row -> Row -> Row

{-
  Given a list of column indices and a list of rows, create a list of lists of rows
  that have been partitioned by the values in the indices. TODO: make sure this is efficient. It's
  probably not, but maybe the lazy map and lazy list will play together well.
 -}
groupRowsUsingIndices :: [Int] -> [Row] -> [[Row]]
groupRowsUsingIndices indices rows = L.elems $ partition project rows where
    partition f xs = L.fromListWith (++) [(f x, [x]) | x <- xs]
    project row = [row V.! i | i <- indices]

{-
  Given a LeafStore and two Timestamps, return all messages taking place between the Timestamps
 -}
getMessagesInTimeRange :: LeafStore -> Timestamp -> Timestamp -> [LogMessage]
getMessagesInTimeRange store timeStart timeEnd = filter
                                                 (\x -> ((x ^. lmTimestamp) >= timeStart) && ((x ^. lmTimestamp) <= timeEnd))
                                                 store
