{-# LANGUAGE TemplateHaskell, DeriveGeneric #-}

module Shared.Thrift.Types.Internal where

import qualified Huba_Types as T

import Data.Int(Int64(), Int32())
import Data.Vector (Vector())
import Data.Text.Lazy (Text())
import qualified Data.HashSet as Set
import qualified Data.HashMap.Lazy as Map

import Data.Ord (comparing)

import Control.Lens.TH

import Data.Hashable
import GHC.Generics

type ColumnName = T.ColumnName
type ServerID = T.ServerID
type Timestamp = Int64
type GroupBy = Vector ColumnName

data ColumnValue = StringValue Text
                 | IntValue Timestamp
                 | StringSet (Set.HashSet Text)
                 | StringVector (Vector Text)
  deriving (Show, Eq, Generic)

data LogMessage = LogMessage { _lmTimestamp :: Timestamp
                             , _lmTable :: Text
                             , _lmColumns :: Map.HashMap ColumnName ColumnValue
                             } deriving (Show, Eq, Generic)

instance Ord LogMessage where
    compare = comparing _lmTimestamp `orComparing` _lmTable
        where orComparing comp f x x' = case comp x x' of
                                              EQ -> comparing f x x'
                                              o -> o

type LogBatch = Vector LogMessage

data LogResponse = LogResponse { _lrCode :: Int32
                               , _lrMessage :: Maybe Text
                               } deriving (Show, Eq, Generic)

data PingResponse = PingResponse { _prCode :: Int32
                                 , _prMessage :: Maybe Text
                                 } deriving (Show, Eq, Generic)

type AggregationFunction = T.AggregationFunction

data ColumnExpression = ColumnExpression { _ceColumn :: ColumnName
                                         , _ceAggregationFunction :: AggregationFunction
                                         } deriving (Show, Eq)

type ComparisonFunction = T.ComparisonFunction

data Condition = Condition { _cColumn :: ColumnName
                           , _cComparisonFunction :: ComparisonFunction
                           , _cValue :: ColumnValue
                           }
  deriving (Show, Eq)

data Query = Query { _qColumnExpressions :: Vector ColumnExpression
                   , _qTable :: Text
                   , _qTimeStart :: Timestamp
                   , _qTimeEnd :: Timestamp
                   , _qConditions :: Maybe (Vector Condition)
                   , _qGroupBy :: Maybe GroupBy
                   , _qOrderBy :: Maybe Int
                   , _qLimit :: Int
                   }
  deriving (Show,Eq)

data ResponseValue = RDoubleValue Double
                   | RStringValue Text
                   | RIntValue Int64
                   | RStringSet (Set.HashSet Text)
                   | RStringVector (Vector Text)
                   | RNull
  deriving (Show, Eq, Generic)

instance Hashable ResponseValue

-- data Row = Row { _rValues :: Vector ResponseValue }
--   deriving (Show, Eq)

type Row = Vector ResponseValue

data QueryResponse = QueryResponse { _qrCode :: Int32
                                   , _qrMessage :: Maybe Text
                                   , _qrRows :: Maybe (Vector Row)
                                   }
  deriving (Show, Eq)



makeLenses ''LogMessage
makeLenses ''LogResponse
makeLenses ''PingResponse
makeLenses ''ColumnExpression
makeLenses ''Condition
makeLenses ''Query
-- makeLenses ''Row
makeLenses ''QueryResponse
