
module LeafNode.Aggregations where

import Shared.Thrift.Types as T
import Shared.Thrift.Interface
import Shared.Comparison (columnValueToResponseValue)

import qualified Data.Vector as V

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
