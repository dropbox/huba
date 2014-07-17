{-# LANGUAGE ViewPatterns #-}
module Shared.Query where

import Shared.Thrift.Types
import Shared.Comparison ()
import Control.Lens
import Data.Ord (comparing)
import Data.List (sortBy)

qOrdering :: Query -> Maybe (Row -> Row -> Ordering)
qOrdering (view qOrderBy -> Just orderBy) = Just $ comparing $ \r -> r ^? ix orderBy
qOrdering _                               = Nothing

orderRows :: Query -> [Row] -> [Row]
orderRows (qOrdering -> Just ordering) = sortBy ordering
orderRows _                            = id
