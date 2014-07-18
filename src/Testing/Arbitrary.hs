{-# LANGUAGE OverloadedStrings #-}
module Testing.Arbitrary where

import Shared.Thrift.Types as T

import Test.QuickCheck
import Control.Applicative ((<$>), (<*>))
import qualified Data.HashMap.Lazy as M
import qualified Data.Vector as V
import qualified Data.Text.Lazy as Text
import Data.List (zipWith)


someof :: [a] -> Gen [a]
someof xs = do
    toTake <- vectorOf (length xs) arbitrary
    return $ select toTake xs

select :: [Bool] -> [a] -> [a]
select bs xs = map snd $ filter (fst) $ zip bs xs

timestampRange = (0, 10000)

tables = [Text.pack $ "table" ++ [l] | l <- ['A'..'E']]

colNames = [Text.pack $ "column" ++ show i | i <- [1..10]]

stringColValues = [Text.pack $ "string_" ++ [l] | l <- ['a'..'p']]

-- TODO: depend on size
instance Arbitrary LogMessage where
    arbitrary = do
      timestamp <- choose timestampRange
      table <- elements tables
      columns <- someof colNames
      colValues <- vectorOf (length columns)
        $ oneof [ StringValue <$> elements stringColValues
                , IntValue <$> arbitrary
                ]
      return $ LogMessage timestamp table (M.fromList $ zip columns colValues)
      


-- TODO: test all aggfns
instance Arbitrary AggregationFunction where
    -- arbitrary = arbitraryBoundedEnum
    arbitrary = elements [CONSTANT, COUNT, MIN, MAX, SUM]

-- TODO: test all compfns
instance Arbitrary ComparisonFunction where
    -- arbitrary = arbitraryBoundedEnum
    arbitrary = elements [T.EQ, T.NEQ, T.GT, T.LT, T.GTE, T.LTE]


-- TODO: write conditions that are more likely to match
instance Arbitrary Condition where
    arbitrary = do
      col <- elements colNames
      compFn <- arbitrary
      value <- oneof [ StringValue <$> elements stringColValues
                     , IntValue <$> arbitrary
                     ]
      return $ Condition col compFn value


someColumnExpressions = do
    columns <- someof colNames
    colAggs <- vectorOf (length columns) arbitrary
    return $ zipWith ColumnExpression columns colAggs

-- instance Arbitrary ColumnExpression where
--     arbitrary = ColumnExpression <$> elements colNames <*> arbitrary


instance Arbitrary Query where
    arbitrary = do
      cols <- someColumnExpressions
      table <- elements tables
      time1 <- choose timestampRange
      time2 <- choose timestampRange
      conditions <- frequency [ (1, return Nothing)
                              , (7, Just . V.fromList <$> listOf arbitrary)
                              ]
      groupBy <- frequency [ (1, return Nothing)
                           , (7, Just . V.fromList <$> someof (map _ceColumn cols))
                           ]
      orderBy <- frequency [ (1, return Nothing)
                           , (7, Just <$> choose (0, length cols - 1))
                           ]
      limit <- choose (0, 500)
      return $ Query (V.fromList cols)
                     table
                     (min time1 time2)
                     (max time1 time2)
                     conditions
                     groupBy
                     orderBy
                     limit


-- data Query = Query { _qColumnExpressions :: Vector ColumnExpression
--                    , _qTable :: Text
--                    , _qTimeStart :: Timestamp
--                    , _qTimeEnd :: Timestamp
--                    , _qConditions :: Maybe (Vector Condition)
--                    , _qGroupBy :: Maybe GroupBy
--                    , _qOrderBy :: Maybe Int
--                    , _qLimit :: Int
--                    }
--   deriving (Show,Eq)
