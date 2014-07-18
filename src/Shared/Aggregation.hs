module Shared.Aggregation (groupBy, groupBy') where

import Shared.Thrift.Types
import Shared.Row (combine)

import Control.Lens
import qualified Data.Vector as V
import qualified Data.HashMap.Lazy as M

-- TODO: let's rename Row to ResponseRow
groupBy :: Maybe GroupBy -> V.Vector ColumnExpression -> [Row] -> [Row]
groupBy Nothing   _    rs = rs
groupBy (Just gs) cols rs = let Just indices = V.mapM (\g -> V.elemIndex (ColumnExpression g CONSTANT) cols) gs
                                getGroup rvs = V.map (rvs V.!) indices in
                              rs & map (\rvs -> (getGroup rvs, rvs))
                                 & M.fromListWith (combine cols)
                                 & M.elems

{-
  Used by the LeafNodes and the Aggregators to aggregate rows.
 -}
groupBy' :: Query -> [Row] -> [Row]
groupBy' Query { _qColumnExpressions = mce, _qGroupBy = group } = groupBy group mce

