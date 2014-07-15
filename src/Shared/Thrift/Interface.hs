{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances, FlexibleInstances, ViewPatterns #-}
module Shared.Thrift.Interface (
  TypeEquiv(..),
  IngestorService(..),
  LeafNodeService(..),
  T.AggregationFunction(..),
  T.ComparisonFunction(..)
) where

import Shared.Thrift.Types
import qualified Huba_Types as T
import qualified LeafNodeService_Iface as T
import qualified IngestorService_Iface as T
import Control.Applicative ((<$>), (<*>))
import Data.Traversable (Traversable(traverse))
import qualified Data.HashMap.Lazy as Map
import Data.Int (Int32)

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

intToInt32 :: Int -> Int32
intToInt32 = fromInteger . toInteger

int32ToInt :: Int32 -> Int
int32ToInt = fromInteger . toInteger

instance TypeEquiv Query T.Query where
    toThrift (Query ce t ts te mc mg mo ml) = T.Query (Just $ toThrift ce) (Just t) (Just ts) (Just te) (toThrift <$> mc) mg (intToInt32 <$> mo) (intToInt32 <$> ml)
    fromThrift (T.Query mce mt mts mte mc mg mo ml) = Query <$> (mce >>= fromThrift) <*> mt <*> mts <*> mte <*> (fromThrift <$> mc) <*> Just mg <*> Just (int32ToInt <$> mo) <*> Just (int32ToInt <$> ml)

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
