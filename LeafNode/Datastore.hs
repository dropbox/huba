module LeafNode.Datastore (LeafStore(), ingestBatch) where

import Shared.Thrift.Interface
import Data.Vector (foldl')
import Data.List (insert)

type LeafStore = [LogMessage]

ingestBatch :: LeafStore -> LogBatch -> LeafStore
ingestBatch = foldl' (flip insert)
