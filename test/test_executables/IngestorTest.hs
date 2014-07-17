{-# LANGUAGE OverloadedStrings, OverloadedLists #-}
module Main where

import qualified Huba_Types as T

import Shared.Thrift.Types
import Shared.Thrift.Interface
import qualified IngestorService_Client as Client

import Thrift
import Thrift.Protocol.Binary
import Thrift.Transport.Handle

import Network
import System.Environment (getArgs)
import Data.Foldable (toList)
import Control.Monad (replicateM_, replicateM)

import Data.Vector as V (fromList)

import qualified Data.Random.Source.DevRandom as R
import qualified Data.Random as R

import Testing.Util (genRandomLogMessage)

main :: IO ()
main = do
  args <- getArgs
  let [numBatches, batchSize] = map (fromIntegral . read) args

  transport  <- hOpen ("localhost" :: String, PortNumber 8000)
  let binProto = BinaryProtocol transport
      protocols = (binProto, binProto)

  replicateM_ numBatches $ do
         -- Get batchSize random messages
         msgs <- replicateM batchSize $ R.runRVar genRandomLogMessage R.DevURandom
         resp <- Client.log protocols $ V.fromList $ map toThrift (toList msgs)
         putStrLn $ "Got response: " ++ show resp

  -- Control.Exception.catch (printf "bad log message" =<< Client.log protocols 42.2)
  --       (\e -> printf "InvalidOperation %s\n" (show (e :: InvalidLogMessageException)))

  tClose transport
