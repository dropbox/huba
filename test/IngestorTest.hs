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

import Data.Sequence
import Data.Text.Lazy

import qualified Data.Random as R
import qualified Data.Random.Extras as RE
import qualified Data.HashMap.Lazy as Map
import qualified Data.Random.Source.DevRandom as R

-- sendLogMessages :: GHC.IO.Handle.Types.Handle -> [LogMessage] -> IO ()
-- sendLogMessages transport msgs = do
--   let binProto = BinaryProtocol transport
--   let protocols = (binProto, binProto)

--   resp <- Client.log protocols $ map toThrift msgs

--   return

main :: IO ()
main = do

  ports <- getArgs

  transport  <- hOpen ("localhost" :: String, PortNumber 8000)
  let binProto = BinaryProtocol transport
  let protocols = (binProto, binProto)

  msg <- R.runRVar genRandomLogMessage R.DevURandom

  resp <- Client.log protocols [toThrift msg]
  putStrLn "Sent log message!"
  print resp

  -- Control.Exception.catch (printf "bad log message" =<< Client.log protocols 42.2)
  --       (\e -> printf "InvalidOperation %s\n" (show (e :: InvalidLogMessageException)))

  -- Close!
  tClose transport


tableChoices = ["tableA", "tableB", "tableC"]



-- TODO: make this a bit more interesting
genRandomLogMessage = do
  ts <- R.uniform 0 100000
  table <- RE.choice tableChoices

  string1 <- RE.sample 10 "abcdefghijklmnopqrstuvwxyz"
  int1 <- R.uniform 0 100

  return $ LogMessage ts table $ Map.fromList [("string1", StringValue $ pack string1),
                                               ("int1", IntValue int1)]
