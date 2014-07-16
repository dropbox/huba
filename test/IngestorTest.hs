{-# LANGUAGE OverloadedStrings, OverloadedLists #-}
module Main where


import Shared.Thrift.Types
import Shared.Thrift.Interface
import qualified IngestorService_Client as Client

import Thrift
import Thrift.Protocol.Binary
import Thrift.Transport.Handle

import Network


main :: IO ()
main = do
  transport  <- hOpen ("localhost" :: String, PortNumber 8000)
  let binProto = BinaryProtocol transport
  let protocols = (binProto, binProto)

  resp <- Client.log protocols [toThrift simpleLogMessage]
  putStrLn "Sent log message!"
  print resp

  -- Control.Exception.catch (printf "bad log message" =<< Client.log protocols 42.2)
  --       (\e -> printf "InvalidOperation %s\n" (show (e :: InvalidLogMessageException)))

  -- Close!
  tClose transport


simpleLogMessage :: LogMessage
simpleLogMessage = LogMessage 0 "some-table" [("key", StringValue "some string value")]
