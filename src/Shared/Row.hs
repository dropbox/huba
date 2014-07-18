{-# LANGUAGE ViewPatterns #-}
module Shared.Row (project, combine) where

import Shared.Thrift.Types

import Control.Lens
import qualified Data.Vector as V
import qualified Data.HashMap.Lazy as M

project :: V.Vector ColumnExpression -> LogMessage -> Row
project cols LogMessage { _lmColumns = colMap }
  = V.map findAndProject cols
  where findAndProject (ColumnExpression name agg) = case name `M.lookup` colMap of
                                                       Nothing -> RNull
                                                       Just colValue -> projectCol agg colValue

projectCol :: AggregationFunction -> ColumnValue -> ResponseValue
projectCol CONSTANT (StringValue v)        = RStringValue v
projectCol CONSTANT (IntValue v)           = RIntValue v
projectCol CONSTANT (StringSet v)          = RStringSet v
projectCol CONSTANT (StringVector v)       = RStringVector v

projectCol COUNT _                         = RIntValue 1

projectCol MIN (IntValue v)                = RIntValue v
projectCol MIN _                           = RNull

projectCol MAX (IntValue v)                = RIntValue v
projectCol MAX _                           = RNull

projectCol SUM (IntValue v)                = RIntValue v
projectCol SUM _                           = RNull

projectCol AVERAGE (IntValue _)            = error "Average not supported"
projectCol AVERAGE _                       = RNull

projectCol SUM_PER_MINUTE (IntValue _)     = error "Sum per minute not supported"
projectCol SUM_PER_MINUTE _                = RNull

projectCol HISTOGRAM _                     = error "Histogram not supported"



combine :: V.Vector ColumnExpression -> Row -> Row -> Row
combine (V.map (view ceAggregationFunction) -> aggFns) = V.zipWith3 combineCol aggFns


combineCol :: AggregationFunction -> ResponseValue -> ResponseValue -> ResponseValue
combineCol CONSTANT v _                        = v

combineCol COUNT (RIntValue v1) (RIntValue v2) = RIntValue $ v1 + v2
combineCol COUNT _              (RIntValue v2) = RIntValue $ v2
combineCol COUNT (RIntValue v1) _              = RIntValue $ v1
combineCol COUNT _              _              = RNull

combineCol MIN (RIntValue v1) (RIntValue v2)   = RIntValue $ min v1 v2
combineCol MIN _              (RIntValue v2)   = RIntValue $ v2
combineCol MIN (RIntValue v1) _                = RIntValue $ v1
combineCol MIN _              _                = RNull

combineCol MAX (RIntValue v1) (RIntValue v2)   = RIntValue $ max v1 v2
combineCol MAX _              (RIntValue v2)   = RIntValue $ v2
combineCol MAX (RIntValue v1) _                = RIntValue $ v1
combineCol MAX _              _                = RNull

combineCol SUM (RIntValue v1) (RIntValue v2)   = RIntValue $ v1 + v2
combineCol SUM _              (RIntValue v2)   = RIntValue $ v2
combineCol SUM (RIntValue v1) _                = RIntValue $ v1
combineCol SUM _              _                = RNull

combineCol AVERAGE _ _                         = error "Average not supported"

combineCol SUM_PER_MINUTE _ _                  = error "Sum per minute not supported"

combineCol HISTOGRAM _ _                       = error "Histogram not supported"
