{-# LANGUAGE OverloadedStrings #-}
module Aggregator.Aggregator where

import Shared.Thrift.Types
import Shared.Query (orderRows)
import Shared.Aggregation (groupBy')

import qualified Data.Vector as V

import Control.Lens
import Control.Lens.TH

import Data.Maybe (mapMaybe)

fanoutFactor :: Int
fanoutFactor = 5

fanout :: V.Vector ServerID -> [V.Vector ServerID]
fanout = chop fanoutFactor
  where chop :: Int -> V.Vector a -> [V.Vector a]
        chop _ xs | V.null xs = []
        chop 0 _              = []
        chop n xs             = slice : chop (n - 1) rest
          where (slice, rest) = V.splitAt sliceSize xs
                sliceSize = ceiling (fromIntegral (V.length xs) / fromIntegral n :: Double)

-- TODO: propagate errors?
-- TODO: don't convert back and forth from a Vector; it's slow
aggregate :: Query -> [QueryResponse] -> QueryResponse
aggregate query rs = QueryResponse 0 Nothing (Just $ V.fromList rows)
  where rows = take limit $ orderRows query aggregatedRows
        limit = query ^. qLimit
        allRows = concatMap V.toList $ mapMaybe (view qrRows) rs
        aggregatedRows = groupBy' query allRows


rootQueryTransform :: Query -> Query
rootQueryTransform = id -- TODO: this


rootResponseTransform :: Maybe QueryResponse -> QueryResponse
rootResponseTransform (Just qr) = qr -- TODO: this
rootResponseTransform Nothing = QueryResponse (-1) (Just "Aggregator returned invalid query.") Nothing
