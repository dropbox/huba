module UnitTests.LeafNode.DatastoreSpec (main, spec) where

import LeafNode.Datastore

import Test.Hspec
-- import Test.QuickCheck

import Common.TestData (makeSpec)

import Shared.Thrift.Types as T


testQueryResponse :: LogBatch -> Query -> QueryResponse -> Expectation
testQueryResponse messages q queryResponse = query messageStore q `shouldBe` queryResponse where
    messageStore = ingestBatch [] messages

spec = makeSpec testQueryResponse

main :: IO ()
main = hspec spec
