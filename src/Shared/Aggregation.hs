module Shared.Aggregation (aggregateRows) where


import Shared.Thrift.Types as T
import Shared.Thrift.Interface
import Shared.Comparison
import Shared.Query (orderRows)

import qualified Data.Vector as V

import Control.Lens
import Control.Lens.TH

import Data.Maybe (isNothing, fromJust, catMaybes)

import qualified Data.Map.Lazy as L

{-
  This is the only method exported by this module.
  Used by the LeafNodes and the Aggregators to aggregate rows.
 -}
aggregateRows :: Query -> [Row] -> [Row] -- TODO: let's rename Row to ResponseRow
aggregateRows q@(Query ce _ _ _ _ groupBy _ _) rows =
    if isNothing groupBy then
        if isSimpleQuery q then
            rows
        else
            [processSingleGroupBy q rows]
    else map (processSingleGroupBy q) (groupRowsUsingIndices groupByIndices rows) where -- TODO: cleanup these lines
        groupByIndices = catMaybes [V.findIndex (\x -> x ^. ceColumn == groupByName) (q ^. qColumnExpressions) | groupByName <- V.toList $ fromJust groupBy]

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

------------------------------------------------------------------------------------------------

type FoldFunction = ResponseValue -> ResponseValue -> ResponseValue

aggConstant :: FoldFunction
aggConstant RNull RNull = RNull
aggConstant RNull x = x
aggConstant v _ = v
initialConstant = RNull

aggCount :: FoldFunction
aggCount v RNull = v
aggCount (RIntValue i1) _ = RIntValue (i1 + 1)
initialCount = RIntValue 0

aggSum :: FoldFunction
aggSum v RNull = v
aggSum (RIntValue i1) (RIntValue i2) = RIntValue (i1 + i2)
initialSum = RIntValue 0

aggMin :: FoldFunction
aggMin v RNull = v
aggMin RNull (RIntValue i) = RIntValue i
aggMin (RIntValue i1) (RIntValue i2) = RIntValue $ min i1 i2
initialMin = RNull

aggMax :: FoldFunction
aggMax v RNull = v
aggMax RNull (RIntValue i) = RIntValue i
aggMax (RIntValue i1) (RIntValue i2) = RIntValue $ max i1 i2
initialMax = RNull

aggFunctionAndInitialValue :: AggregationFunction -> (FoldFunction, ResponseValue)
aggFunctionAndInitialValue CONSTANT = (aggConstant, initialConstant)
aggFunctionAndInitialValue COUNT = (aggCount, initialCount)
aggFunctionAndInitialValue MIN = (aggMin, initialMin)
aggFunctionAndInitialValue MAX = (aggMax, initialMax)
aggFunctionAndInitialValue SUM = (aggSum, initialSum)
aggFunctionAndInitialValue AVERAGE =  undefined
aggFunctionAndInitialValue SUM_PER_MINUTE =  undefined
aggFunctionAndInitialValue HISTOGRAM = undefined
