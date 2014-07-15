module Shared.Comparison where

import Shared.Thrift.Types

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
