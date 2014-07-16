{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

module LeafNode.DatastoreSpec (main, spec) where

import LeafNode.Datastore

import Test.Hspec
-- import Test.QuickCheck

import Shared.Thrift.Types as T
import Shared.Thrift.Interface




main :: IO ()
main = hspec spec


spec = do
  describe "Basic ingesting" $
    it "stores a message in a LeafStore and lets you query it" $
       testQueryResponse (ingestBatch [] [simpleMsg])
                         (basicTimeRangeQuery 0 50)
                         (successfulQueryResponse [Row [RStringValue "s1"]])

  describe "Time queries" $ do
    it "returns messages in the middle of a time range" $
       testQueryResponse allMessagesStore
                         (basicTimeRangeQuery 15 25)
                         (successfulQueryResponse [Row [RStringValue "s2"]])

    it "returns all messages to the right" $
       testQueryResponse allMessagesStore
                         (basicTimeRangeQuery 15 100)
                         (successfulQueryResponse [Row [RStringValue "s2"], Row [RStringValue "s3"], Row [RStringValue "s3"]])

  describe "Simple projection queries" $
    it "should project out columns" $
       testQueryResponse allMessagesStore
                         (Query [ColumnExpression "int1" CONSTANT, ColumnExpression "string1" CONSTANT] "some-table" 0 100 Nothing Nothing Nothing 100)
                         (successfulQueryResponse [Row [RIntValue 42, RStringValue "s1"],
                                                  Row [RIntValue 100, RStringValue "s2"],
                                                  Row [RIntValue 3, RStringValue "s3"],
                                                  Row [RIntValue 15, RStringValue "s3"]])

  describe "Aggregations" $ do
    it "can do a count aggregation" $
       testQueryResponse allMessagesStore
                         (Query [ColumnExpression "int1" COUNT, ColumnExpression "string1" CONSTANT] "some-table" 0 100 Nothing Nothing Nothing 100)
                         (successfulQueryResponse [Row [RIntValue 4, RStringValue "s1"]])

    it "can do a min aggregation" $
       testQueryResponse allMessagesStore
                         (Query [ColumnExpression "int1" MIN, ColumnExpression "string1" CONSTANT] "some-table" 0 100 Nothing Nothing Nothing 100)
                         (successfulQueryResponse [Row [RIntValue 3, RStringValue "s1"]])

    it "can do a max aggregation" $
       testQueryResponse allMessagesStore
                         (Query [ColumnExpression "int1" MAX, ColumnExpression "string1" CONSTANT] "some-table" 0 100 Nothing Nothing Nothing 100)
                         (successfulQueryResponse [Row [RIntValue 100, RStringValue "s1"]])

    it "can do a sum aggregation" $
       testQueryResponse allMessagesStore
                         (Query [ColumnExpression "int1" SUM, ColumnExpression "string1" CONSTANT] "some-table" 0 100 Nothing Nothing Nothing 100)
                         (successfulQueryResponse [Row [RIntValue 160, RStringValue "s1"]])

  describe "GroupBys" $ do
    it "can group by a single string column" $
       testQueryResponse allMessagesStore
                         (Query [ColumnExpression "int1" SUM, ColumnExpression "string1" CONSTANT] "some-table" 0 100 Nothing (Just ["string1"]) Nothing 100)
                         (successfulQueryResponse [Row [RIntValue 42, RStringValue "s1"],
                                                   Row [RIntValue 100, RStringValue "s2"],
                                                   Row [RIntValue 18, RStringValue "s3"]])

    it "can group by a string and int column together" $
       testQueryResponse groupByMessageStore
                         (Query [ColumnExpression "string1" CONSTANT, ColumnExpression "int1" CONSTANT] "some-table" 0 100 Nothing (Just ["int1"]) Nothing 100)
                         (successfulQueryResponse [Row [RStringValue "s1", RIntValue 20],
                                                   Row [RStringValue "s2", RIntValue 30],
                                                   Row [RStringValue "s2", RIntValue 50]])

  describe "Aggregations with GroupBys" $ do
    it "can group by a string while aggregating an int" $
       testQueryResponse groupByMessageStore
                         (Query [ColumnExpression "string1" CONSTANT, ColumnExpression "int1" SUM] "some-table" 0 100 Nothing (Just ["string1"]) Nothing 100)
                         (successfulQueryResponse [Row [RStringValue "s1", RIntValue 40],
                                                   Row [RStringValue "s2", RIntValue 110]])

-- Helper functions --

testQueryResponse :: LeafStore -> Query -> QueryResponse -> Expectation
testQueryResponse messageStore q queryResponse = query messageStore q `shouldBe` queryResponse

basicTimeRangeQuery :: Timestamp -> Timestamp -> Query
basicTimeRangeQuery t1 t2 = Query [ColumnExpression "string1" CONSTANT] "some-table" t1 t2 Nothing Nothing Nothing 100

successfulQueryResponse rows = QueryResponse 0 Nothing (Just rows)

-- Data --

simpleMsg = LogMessage 10 "some-table" [("string1", StringValue "s1"), ("int1", IntValue 42), ("vector1", StringVector ["a", "b", "c"])]

allMessagesStore = ingestBatch [] [
  LogMessage 10 "some-table" [("string1", StringValue "s1"), ("int1", IntValue 42), ("vector1", StringVector ["a", "b", "c"])],
  LogMessage 20 "some-table" [("string1", StringValue "s2"), ("int1", IntValue 100), ("vector2", StringVector ["b", "c"])],
  LogMessage 30 "some-table" [("string1", StringValue "s3"), ("int1", IntValue 3), ("vector3", StringVector ["x", "y", "z"])],
  LogMessage 40 "some-table" [("string1", StringValue "s3"), ("int1", IntValue 15), ("vector3", StringVector ["p", "q", "r"])]
  ]

groupByMessageStore = ingestBatch [] [
 LogMessage 10 "some-table" [("string1", StringValue "s1"), ("int1", IntValue 20)],
 LogMessage 20 "some-table" [("string1", StringValue "s1"), ("int1", IntValue 20)],
 LogMessage 30 "some-table" [("string1", StringValue "s2"), ("int1", IntValue 30)],
 LogMessage 40 "some-table" [("string1", StringValue "s2"), ("int1", IntValue 30)],
 LogMessage 40 "some-table" [("string1", StringValue "s2"), ("int1", IntValue 50)]
 ]
