{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances, FlexibleInstances, ViewPatterns #-}
module Shared.Thrift.Interface where

import qualified Huba_Types as T
import qualified LeafNodeService_Iface as T
import qualified IngestorService_Iface as T
import qualified Data.HashSet as Set
import qualified Data.HashMap.Lazy as Map
import qualified Data.Vector as Vector
import Data.Text.Lazy
import Data.Int(Int64(), Int32())
import Control.Applicative ((<$>), (<*>))
import Data.Ord
import Data.Traversable (Traversable(traverse))

type ColumnName = T.ColumnName

data ColumnValue = StringValue Text
                 | IntValue Int64
                 | StringSet (Set.HashSet Text)
                 | StringVector (Vector.Vector Text)
  deriving (Show, Eq)

data LogMessage = LogMessage { lmTimestamp :: Int64
                             , lmTable :: Text
                             , lmColumns :: Map.HashMap Text ColumnValue
                             } deriving (Show, Eq)

instance Ord LogMessage where
    compare = comparing lmTimestamp `orComparing` lmTable
        where orComparing comp f x x' = case comp x x' of
                                              EQ -> comparing f x x'
                                              o -> o

type LogBatch = Vector.Vector LogMessage

data LogResponse = LogResponse { lrCode :: Int32
                              , lrMessage :: Text
                              } deriving Show

type AggregationFunction = T.AggregationFunction

data ColumnExpression = ColumnExpression { ceColumn :: Text
                                         , ceAggregationFunction :: AggregationFunction
                                         } deriving (Show, Eq)

type ComparisonFunction = T.ComparisonFunction

data Condition = Condition { cColumn :: Text
                           , cComparisonFunction :: ComparisonFunction
                           , cValue :: ColumnValue
                           }
  deriving (Show, Eq)

data Query = Query { qColumnExpressions :: (Vector.Vector ColumnExpression)
                   , qTable :: Text
                   , qTimeStart :: Int64
                   , qTimeEnd :: Int64
                   , qConditions :: Maybe (Vector.Vector Condition)
                   , qGroupBy :: Maybe (Vector.Vector Text)
                   , qOrderBy :: Maybe Int32
                   , qLimit :: Maybe Int32
                   }
  deriving (Show,Eq)

data ResponseValue = RStringValue Text
                   | RIntValue Int64
                   | RStringSet (Set.HashSet Text)
                   | RStringVector (Vector.Vector Text)
                   | RDoubleValue Double
                   | RNull
  deriving (Show, Eq)

data Row = Row { rValues :: Vector.Vector ResponseValue }
  deriving (Show, Eq)

data QueryResponse = QueryResponse { qrCode :: Int32
                                   , qrMessage :: Maybe Text
                                   , qrRows :: Maybe (Vector.Vector Row)
                                   }
  deriving (Show, Eq)


---------------------

class TypeEquiv haskType thriftType | haskType -> thriftType, thriftType -> haskType where
    toThrift :: haskType -> thriftType
    fromThrift :: thriftType -> Maybe haskType


instance TypeEquiv ColumnValue T.ColumnValue where
    toThrift (StringValue t)  = T.ColumnValue (Just t) Nothing  Nothing  Nothing
    toThrift (IntValue i)     = T.ColumnValue Nothing  (Just i) Nothing  Nothing
    toThrift (StringSet s)    = T.ColumnValue Nothing  Nothing  (Just s) Nothing
    toThrift (StringVector v) = T.ColumnValue Nothing  Nothing  Nothing  (Just v)

    fromThrift (T.ColumnValue Nothing  Nothing  Nothing  Nothing)  = Nothing
    fromThrift (T.ColumnValue (Just t) Nothing  Nothing  Nothing)  = Just $ StringValue t
    fromThrift (T.ColumnValue Nothing  (Just i) Nothing  Nothing)  = Just $ IntValue i
    fromThrift (T.ColumnValue Nothing  Nothing  (Just s) Nothing)  = Just $ StringSet s
    fromThrift (T.ColumnValue Nothing  Nothing  Nothing  (Just v)) = Just $ StringVector v
    fromThrift _                                                   = Nothing

instance TypeEquiv LogMessage T.LogMessage where
    toThrift (LogMessage time table cols) = T.LogMessage (Just time) (Just table) (Just $ Map.map toThrift cols)
    fromThrift (T.LogMessage mTime mTable mCols) = LogMessage <$> mTime <*> mTable <*> (mCols >>= Map.traverseWithKey (const fromThrift))

instance TypeEquiv LogResponse T.LogResponse where
    toThrift (LogResponse c m) = T.LogResponse (Just c) (Just m)
    fromThrift (T.LogResponse mc mm) = LogResponse <$> mc <*> mm


instance TypeEquiv ColumnExpression T.ColumnExpression where
    toThrift (ColumnExpression c af) = T.ColumnExpression (Just c) (Just af)
    fromThrift (T.ColumnExpression mc maf) = ColumnExpression <$> mc <*> maf

instance TypeEquiv Condition T.Condition where
    toThrift (Condition c cf v) = T.Condition (Just c) (Just cf) (Just $ toThrift v)
    fromThrift (T.Condition mc mcf mv) = Condition <$> mc <*> mcf <*> (mv >>= fromThrift)

instance TypeEquiv Query T.Query where
    toThrift (Query ce t ts te mc mg mo ml) = T.Query (Just $ toThrift ce) (Just t) (Just ts) (Just te) (toThrift <$> mc) mg mo ml
    fromThrift (T.Query mce mt mts mte mc mg mo ml) = Query <$> (mce >>= fromThrift) <*> mt <*> mts <*> mte <*> (fromThrift <$> mc) <*> Just mg <*> Just mo <*> Just ml

instance TypeEquiv ResponseValue T.ResponseValue where
    toThrift (RStringValue t)  = T.ResponseValue (Just t) Nothing  Nothing  Nothing  Nothing  Nothing
    toThrift (RIntValue i)     = T.ResponseValue Nothing  (Just i) Nothing  Nothing  Nothing  Nothing
    toThrift (RStringSet s)    = T.ResponseValue Nothing  Nothing  (Just s) Nothing  Nothing  Nothing
    toThrift (RStringVector v) = T.ResponseValue Nothing  Nothing  Nothing  (Just v) Nothing  Nothing
    toThrift (RDoubleValue d)  = T.ResponseValue Nothing  Nothing  Nothing  Nothing  (Just d) Nothing
    toThrift (RNull)           = T.ResponseValue Nothing  Nothing  Nothing  Nothing  Nothing  (Just True)

    fromThrift (T.ResponseValue Nothing  Nothing  Nothing  Nothing  Nothing  Nothing)  = Nothing
    fromThrift (T.ResponseValue (Just t) Nothing  Nothing  Nothing  Nothing  Nothing)  = Just $ RStringValue t
    fromThrift (T.ResponseValue Nothing  (Just i) Nothing  Nothing  Nothing  Nothing)  = Just $ RIntValue i
    fromThrift (T.ResponseValue Nothing  Nothing  (Just s) Nothing  Nothing  Nothing)  = Just $ RStringSet s
    fromThrift (T.ResponseValue Nothing  Nothing  Nothing  (Just v) Nothing  Nothing)  = Just $ RStringVector v
    fromThrift (T.ResponseValue Nothing  Nothing  Nothing  Nothing  (Just d) Nothing)  = Just $ RDoubleValue d
    fromThrift (T.ResponseValue Nothing  Nothing  Nothing  Nothing  Nothing  (Just _)) = Just $ RNull
    fromThrift _                                                   = Nothing

instance TypeEquiv Row T.Row where
    toThrift (Row v) = T.Row (Just $ toThrift v)
    fromThrift (T.Row mv) = Row <$> (mv >>= fromThrift)

instance TypeEquiv QueryResponse T.QueryResponse where
    toThrift (QueryResponse c m r) = T.QueryResponse (Just c) m (toThrift <$> r)
    fromThrift (T.QueryResponse mc mm mr) = QueryResponse <$> mc <*> Just mm <*> (Just $ mr >>= fromThrift)


instance (TypeEquiv t1 t2, Traversable c) => TypeEquiv (c t1) (c t2) where
    toThrift = fmap toThrift
    fromThrift = traverse fromThrift


-------------------------------

class LeafNodeService t where
    logLeaf :: t -> LogBatch -> IO LogResponse
    queryLeaf :: t -> Query -> IO QueryResponse

instance (LeafNodeService t) => T.LeafNodeService_Iface t where
    log h ((>>= fromThrift) -> Just message) = toThrift <$> logLeaf h message
    log _ _ = return $ toThrift $ LogResponse (-1) "Invalid LogMessage!"

    query h ((>>= fromThrift) -> Just q) = toThrift <$> queryLeaf h q
    query _ _ = return $ toThrift $ QueryResponse (-1) (Just "Invalid Query!") Nothing


class IngestorService t where
    logIngest :: t -> LogBatch -> IO LogResponse

instance (IngestorService t) => T.IngestorService_Iface t where
    log h ((>>= fromThrift) -> Just message) = toThrift <$> logIngest h message
    log _ _ = return $ toThrift $ LogResponse (-1) "Invalid LogMessage!"
