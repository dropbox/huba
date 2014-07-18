{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances, FlexibleInstances, ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Shared.Thrift.Interface where

import Shared.Thrift.Types
import qualified Huba_Types as T
import qualified CommonService_Iface as T
import qualified LeafNodeService_Iface as T
import qualified IngestorService_Iface as T
import qualified AggregatorService_Iface as T
import qualified InternalAggregatorService_Iface as T

import Data.Vector (Vector())
import Control.Applicative ((<$>), (<*>))
import Data.Traversable (Traversable(traverse))
import qualified Data.HashMap.Lazy as Map
import Data.Int (Int32)

import System.Log.Logger

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
    toThrift (LogResponse c m) = T.LogResponse (Just c) m
    fromThrift (T.LogResponse mc mm) = LogResponse <$> mc <*> return mm

instance TypeEquiv PingResponse T.PingResponse where
    toThrift (PingResponse c m) = T.PingResponse (Just c) m
    fromThrift (T.PingResponse mc mm) = PingResponse <$> mc <*> return mm

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
    toThrift (Query ce t ts te mc mg mo l) = T.Query (Just $ toThrift ce) (Just t) (Just ts) (Just te) (toThrift <$> mc) mg (intToInt32 <$> mo) (Just $ intToInt32 l)
    fromThrift (T.Query mce mt mts mte mc mg mo ml) = Query <$> (mce >>= fromThrift)
                                                            <*> mt
                                                            <*> mts
                                                            <*> mte
                                                            <*> return (mc >>= fromThrift)
                                                            <*> Just mg
                                                            <*> Just (int32ToInt <$> mo)
                                                            <*> (int32ToInt <$> ml)

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


instance TypeEquiv QueryResponse T.QueryResponse where
    toThrift (QueryResponse c m r) = T.QueryResponse (Just c) m (toThrift <$> r)
    fromThrift (T.QueryResponse mc mm mr) = QueryResponse <$> mc <*> Just mm <*> (Just $ mr >>= fromThrift)


instance (TypeEquiv t1 t2, Traversable c) => TypeEquiv (c t1) (c t2) where
    toThrift = fmap toThrift
    fromThrift = traverse fromThrift

-------------------------------

instance (CommonService t) => T.CommonService_Iface t where
    ping x = return $ commonPing x

class CommonService t where
    commonPing :: t -> T.PingResponse
    commonPing _ = T.PingResponse (Just 0) Nothing

-------------------------------

class LeafNodeService t where
    logLeaf :: t -> LogBatch -> IO LogResponse
    queryLeaf :: t -> Query -> IO QueryResponse

instance (LeafNodeService t, CommonService t) => T.LeafNodeService_Iface t where
    log h ((>>= fromThrift) -> Just message) = toThrift <$> logLeaf h message
    log _ _ = return $ toThrift $ LogResponse (-1) (Just "Invalid LogMessage!")

    query h ((>>= fromThrift) -> Just q) = toThrift <$> queryLeaf h q
    query _ q = do
      noticeM "----------***********!!!!!!!!!!!%%%%%%%%%%%"  $ show q
      return $ toThrift $ QueryResponse (-1) (Just "Invalid Query!") Nothing

-------------------------------

class IngestorService t where
    logIngest :: t -> LogBatch -> IO LogResponse

instance (IngestorService t, CommonService t) => T.IngestorService_Iface t where
    log h ((>>= fromThrift) -> Just message) = toThrift <$> logIngest h message
    log _ _ = return $ toThrift $ LogResponse (-1) (Just "Invalid LogMessage!")

-------------------------------

class AggregatorService t where
    queryAggregator :: t -> Query -> IO QueryResponse

instance (AggregatorService t, CommonService t) => T.AggregatorService_Iface t where
    query h ((>>= fromThrift) -> Just q) = toThrift <$> queryAggregator h q
    query _ _ = return $ toThrift $ QueryResponse (-1) (Just "Invalid Query!") Nothing

-------------------------------

class InternalAggregatorService t where
    queryInternalAggregator :: t -> Query -> Vector ServerID -> IO QueryResponse

instance (InternalAggregatorService t, CommonService t) => T.InternalAggregatorService_Iface t where
    queryInternal h ((>>= fromThrift) -> Just q) (Just ids) = toThrift <$> queryInternalAggregator h q ids
    queryInternal _ _ _ = return $ toThrift $ QueryResponse (-1) (Just "Invalid Query!") Nothing
