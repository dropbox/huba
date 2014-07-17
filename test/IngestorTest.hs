{-# LANGUAGE OverloadedStrings, OverloadedLists #-}
module Main where


import Shared.Thrift.Types
import Shared.Thrift.Interface
import qualified IngestorService_Client as Client

import Thrift
import Thrift.Protocol.Binary
import Thrift.Transport.Handle

import System.Environment (getArgs)

import Network

import qualified Huba_Types as T

import qualified Data.Sequence as S
import qualified Data.Text.Lazy as L

import qualified Data.Random as R
import qualified Data.Random.Extras as RE
import qualified Data.HashMap.Lazy as Map
import qualified Data.Random.Source.DevRandom as R

import Data.Foldable (toList)

import Control.Monad (replicateM_, replicateM)

import Data.Vector as V (fromList)

main :: IO ()
main = do
  args <- getArgs
  let [numBatches, batchSize] = map (fromIntegral . read) args

  transport  <- hOpen ("localhost" :: String, PortNumber 8000)
  let binProto = BinaryProtocol transport
  let protocols = (binProto, binProto)

  replicateM_ numBatches $ do
         -- Get batchSize random messages
         msgs <- replicateM batchSize $ R.runRVar genRandomLogMessage R.DevURandom
         resp <- Client.log protocols $ V.fromList $ map toThrift (toList msgs)
         putStrLn $ "Got response: " ++ show resp

  -- Control.Exception.catch (printf "bad log message" =<< Client.log protocols 42.2)
  --       (\e -> printf "InvalidOperation %s\n" (show (e :: InvalidLogMessageException)))

  tClose transport


tableChoices = ["tableA", "tableB", "tableC"]

genRandomLogMessage = do
  ts <- R.uniform 0 100000
  table <- RE.choice tableChoices

  string1 <- RE.sample 10 ['a'..'z']
  int1 <- R.uniform 0 100

  -- numStringsInVector <- R.uniform 0 10
  -- TODO: make a vector with numStringsInVector random strings and add it to the message with some probability
  -- TODO: make there be a chance of adding a random set of strings
  return $ LogMessage ts table $ Map.fromList [("string1", StringValue $ L.pack string1),
                                               ("int1", IntValue int1)]
