
columnValueEq :: ColumnValue -> ColumnValue -> Bool
columnValueEq (StringValue s1) (StringValue s2) = s1 (==) s2
