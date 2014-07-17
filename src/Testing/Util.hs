{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

module Testing.Util where

import Shared.Thrift.Types
import Shared.Thrift.Interface

import qualified Data.Random as R
import qualified Data.Random.Extras as RE
import qualified Data.HashMap.Lazy as Map

import qualified Data.Text.Lazy as L


genRandomLogMessage = do
  ts <- R.uniform 0 100000
  table <- RE.choice ["tableA", "tableB", "tableC"]

  string1 <- RE.sample 10 ['a'..'z']
  int1 <- R.uniform 0 100

  -- numStringsInVector <- R.uniform 0 10
  -- TODO: make a vector with numStringsInVector random strings and add it to the message with some probability
  -- TODO: make there be a chance of adding a random set of strings
  return $ LogMessage ts table $ Map.fromList [("string1", StringValue $ L.pack string1),
                                               ("int1", IntValue int1)]
