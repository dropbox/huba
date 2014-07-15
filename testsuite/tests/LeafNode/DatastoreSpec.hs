{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

module DatastoreSpec () where

import Test.Hspec
-- import Test.QuickCheck

import LeafNode.Datastore
import Shared.Thrift.Interface


main :: IO ()
main = hspec spec


spec = do
  describe "ingestBatch" $
    it "stores a message in a LeafStore" $
       ingestBatch [] [lm1] == [lm1]

    it "stores messages in sorted order" $
       ingestBatch [] [lm2, lm3, lm1] == [lm1, lm2, lm3]

  describe "getMessagesInTimeRange" $
    it "returns the set of messages in a given time range" $
       getMessagesInTimeRange [lm1, lm2, lm3] 15 25 == [lm2]


lm1 = LogMessage 10 "some-table" [("key", StringValue "some string value")]
lm2 = LogMessage 20 "some-table" [("key", StringValue "a second string value")]
lm3 = LogMessage 30 "some-table" [("key", StringValue "a third string value")]
