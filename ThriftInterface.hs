{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module ThriftInterface where

import qualified Huba_Types as T
import qualified Data.HashSet as Set
import qualified Data.HashMap.Lazy as Map
import qualified Data.Vector as Vector
import Data.Text.Lazy
import Data.Int(Int64(), Int32())
import Control.Applicative ((<$>), (<*>))

type ColumnName = T.ColumnName

data ColumnValue = StringValue Text
                 | IntValue Int64
                 | StringSet (Set.HashSet Text)
                 | StringVector (Vector.Vector Text)

data LogMessage = LogMessage { lmTimestamp :: Int64
                             , lmColumns :: Map.HashMap Text ColumnValue
                             }

data LogResponse = LogResponse { lrCode :: Int32
                              , lrMessage :: Text
                              }

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
    toThrift (LogMessage t c) = T.LogMessage (Just $ Map.map toThrift c) (Just t)
    fromThrift (T.LogMessage mc mt) = LogMessage <$> mt <*> (mc >>= Map.traverseWithKey ((fromThrift :: T.ColumnValue -> Maybe ColumnValue) . const))

instance TypeEquiv LogResponse T.LogResponse where
    toThrift (LogResponse c m) = T.LogResponse (Just c) (Just m)
    fromThrift (T.LogResponse mc mm) = LogResponse <$> mc <*> mm
