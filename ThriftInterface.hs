{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances, FlexibleInstances, ViewPatterns #-}
module ThriftInterface where

import qualified Huba_Types as T
import qualified LeafNodeService_Iface as T
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
                             }
  deriving (Show, Eq)

instance Ord LogMessage where
    compare = comparing lmTimestamp `orComparing` lmTable
        where orComparing comp f x x' = case comp x x' of
                                              EQ -> comparing f x x'
                                              o -> o

data LogResponse = LogResponse { lrCode :: Int32
                              , lrMessage :: Text
                              }
  deriving Show

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


instance (TypeEquiv t1 t2, Traversable c) => TypeEquiv (c t1) (c t2) where
    toThrift = fmap toThrift
    fromThrift = traverse fromThrift


class LeafNodeService t where
    log :: t -> Vector.Vector LogMessage -> IO LogResponse


instance (LeafNodeService t) => T.LeafNodeService_Iface t where
    log h ((>>= fromThrift) -> Just message) = toThrift <$> ThriftInterface.log h message
    log _ _ = return $ toThrift $ LogResponse (-1) "Invalid LogMessage!"
