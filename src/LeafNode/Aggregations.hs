
module LeafNode.Aggregations where

import Shared.Thrift.Types as T
import Shared.Thrift.Interface
import Shared.Comparison (columnValueToResponseValue)

import qualified Data.Vector as V

type FoldFunction = ResponseValue -> Maybe ColumnValue -> ResponseValue

aggConstant :: FoldFunction
aggConstant RNull Nothing = RNull
aggConstant RNull (Just x) = columnValueToResponseValue x
aggConstant v _ = v
initialConstant = RNull

aggCount :: FoldFunction
aggCount v Nothing = v
aggCount (RIntValue i1) (Just (IntValue i2)) = RIntValue (i1 + i2)
initialCount = RIntValue 0

aggSum :: FoldFunction
aggSum v Nothing = v
aggSum (RIntValue i1) (Just (IntValue i2)) = RIntValue (i1 + i2)
initialSum = RIntValue 0

aggFunctionAndInitialValue :: AggregationFunction -> (FoldFunction, ResponseValue)
aggFunctionAndInitialValue CONSTANT = (aggConstant, initialConstant)
aggFunctionAndInitialValue COUNT = (aggCount, initialCount)
aggFunctionAndInitialValue MIN = undefined
aggFunctionAndInitialValue MAX = undefined
aggFunctionAndInitialValue SUM = (aggSum, initialSum)
aggFunctionAndInitialValue AVERAGE =  undefined
aggFunctionAndInitialValue SUM_PER_MINUTE =  undefined
aggFunctionAndInitialValue HISTOGRAM = undefined
