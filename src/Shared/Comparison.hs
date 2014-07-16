module Shared.Comparison where

import Shared.Thrift.Types as T
import Control.Lens.TH
import Control.Lens

import Data.Maybe (fromMaybe)

{-
  Given a Condition, generate a predicate on LogMessages to decide
  whether the LogMessage matches the condition
 -}
makeCondition :: Condition -> (LogMessage -> Bool)
makeCondition (Condition col comp val) msg = let compFn = case comp of T.EQ -> columnValueEQ
                                                                       T.NEQ -> columnValueNEQ

                                                                       T.GT -> columnValueGT
                                                                       T.LT -> columnValueLT
                                                                       T.GTE -> columnValueGTE
                                                                       T.LTE -> columnValueLTE

                                                                       T.REGEXP_EQ -> columnValueREGEXPEQ in
  fromMaybe False $ do
    colValue <- getColFromMessage col msg
    return (colValue `compFn` val)

getColFromMessage :: ColumnName -> LogMessage -> Maybe ColumnValue
getColFromMessage name msg = msg ^. lmColumns . at name

columnValueEQ :: ColumnValue -> ColumnValue -> Bool
columnValueEQ (StringValue v1)  (StringValue v2)  = v1 == v2
columnValueEQ (IntValue v1)     (IntValue v2)     = v1 == v2
columnValueEQ (StringSet v1)    (StringSet v2)    = v1 == v2
columnValueEQ (StringVector v1) (StringVector v2) = v1 == v2
columnValueEQ _                 _                 = False

columnValueNEQ :: ColumnValue -> ColumnValue -> Bool
columnValueNEQ (StringValue v1)  (StringValue v2)  = v1 /= v2
columnValueNEQ (IntValue v1)     (IntValue v2)     = v1 /= v2
columnValueNEQ (StringSet v1)    (StringSet v2)    = v1 /= v2
columnValueNEQ (StringVector v1) (StringVector v2) = v1 /= v2
columnValueNEQ _                 _                 = False

columnValueGT :: ColumnValue -> ColumnValue -> Bool
columnValueGT (StringValue v1)  (StringValue v2)  = v1 > v2
columnValueGT (IntValue v1)     (IntValue v2)     = v1 > v2
columnValueGT (StringSet v1)    (StringSet v2)    = False
columnValueGT (StringVector v1) (StringVector v2) = False
columnValueGT _                 _                 = False

columnValueLT :: ColumnValue -> ColumnValue -> Bool
columnValueLT (StringValue v1)  (StringValue v2)  = v1 < v2
columnValueLT (IntValue v1)     (IntValue v2)     = v1 < v2
columnValueLT (StringSet v1)    (StringSet v2)    = False
columnValueLT (StringVector v1) (StringVector v2) = False
columnValueLT _                 _                 = False

columnValueGTE :: ColumnValue -> ColumnValue -> Bool
columnValueGTE (StringValue v1)  (StringValue v2)  = v1 >= v2
columnValueGTE (IntValue v1)     (IntValue v2)     = v1 >= v2
columnValueGTE (StringSet v1)    (StringSet v2)    = False
columnValueGTE (StringVector v1) (StringVector v2) = False
columnValueGTE _                 _                 = False

columnValueLTE :: ColumnValue -> ColumnValue -> Bool
columnValueLTE (StringValue v1)  (StringValue v2)  = v1 <= v2
columnValueLTE (IntValue v1)     (IntValue v2)     = v1 <= v2
columnValueLTE (StringSet v1)    (StringSet v2)    = False
columnValueLTE (StringVector v1) (StringVector v2) = False
columnValueLTE _                 _                 = False

columnValueREGEXPEQ :: ColumnValue -> ColumnValue -> Bool
columnValueREGEXPEQ _            _                 = False


instance Ord ResponseValue where
    RDoubleValue d1 `compare` RDoubleValue d2 = d1 `compare` d2
    RIntValue i1    `compare` RIntValue i2    = i1 `compare` i2
    RStringValue t1 `compare` RStringValue t2 = t1 `compare` t2
    RStringSet _    `compare` RStringSet _    = Prelude.EQ
    RStringVector _ `compare` RStringVector _ = Prelude.EQ

    -- Note that we need to define this for mixed types like RStringValue compared with RIntValue.
    -- Let's just impose an absolute order on types, i.e.
    -- RDoubleValue > RIntValue > RStringValue > RStringSet > RStringVector > RNull
    -- How can we do this concisely? I.e. not like the following:

    RDoubleValue _ `compare` RStringValue _ = Prelude.GT
    RDoubleValue _ `compare` RIntValue _ = Prelude.GT
    RDoubleValue _ `compare` RStringSet _ = Prelude.GT
    RDoubleValue _ `compare` RStringVector _ = Prelude.GT
    RDoubleValue _ `compare` RNull = Prelude.GT

    -- TODO...


columnValueToResponseValue :: ColumnValue -> ResponseValue
columnValueToResponseValue (StringValue s) = RStringValue s
columnValueToResponseValue (IntValue i) = RIntValue i
columnValueToResponseValue (StringSet s) = RStringSet s
columnValueToResponseValue (StringVector s) = RStringVector s
