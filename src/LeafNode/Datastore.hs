{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

module LeafNode.Datastore (LeafStore(), ingestBatch, query) where

import Shared.Thrift.Types as T
import Shared.Thrift.Interface
import Shared.Comparison

import qualified Data.Vector as V
import Data.List (insert, sortBy)

import Control.Lens
import Data.Maybe (fromMaybe)

import Data.Int (Int32)

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

getSortByFn :: Int -> (Row -> Row -> Ordering)
getSortByFn i (Row vals1) (Row vals2) = compare (vals1 V.! i) (vals2 V.! i)

processRows :: Query -> [LogMessage] -> [Row] -- TODO: let's rename Row to ResponseRow
processRows q rows = undefined


query :: LeafStore -> Query -> QueryResponse
query store q = QueryResponse 0 (Just "asdf") (Just $ V.fromList responseRows)
    where
      -- Get the rows in the desired time range
      rowsInTimeRange = getMessagesInTimeRange store (q ^. qTimeStart) (q ^. qTimeEnd)

      filterFn = let conditionFn = case q ^. qConditions of
                                     Nothing -> const True
                                     Just conditions -> makeConditions $ V.toList conditions in
                 filter conditionFn
                 -- TODO: let's make conditions a [Condition] instead of a Vector Condition, yes?

      processFn = processRows q

      sortFn = case q ^. qOrderBy of
                 Nothing -> id
                 Just orderByIndex -> sortBy (getSortByFn orderByIndex)

      limitFn = case q ^. qLimit of
                  Nothing -> id
                  Just n -> take n

      responseRows = (limitFn . sortFn . processFn . filterFn) rowsInTimeRange
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
