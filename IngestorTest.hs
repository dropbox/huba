{-# LANGUAGE OverloadedStrings, OverloadedLists #-}
module Main where


import Huba_Types

-- import Ingestor
-- import Ingestor_Iface
import qualified IngestorService_Client as Client

import Thrift
import Thrift.Protocol.Binary
-- import Thrift.Transport
import Thrift.Transport.Handle
-- import Thrift.Server

-- import Control.Exception
-- import Data.Maybe
-- import Data.Text.Lazy
-- import Text.Printf
import Network


main :: IO ()
main = do
  transport  <- hOpen ("localhost" :: String, PortNumber 9090)
  let binProto = BinaryProtocol transport
  let protocols = (binProto, binProto)

  resp <- Client.log protocols [simpleLogMessage]
  putStrLn "Sent log message!"
  print resp

  -- Control.Exception.catch (printf "bad log message" =<< Client.log protocols 42.2)
  --       (\e -> printf "InvalidOperation %s\n" (show (e :: InvalidLogMessageException)))

  -- Close!
  tClose transport


simpleLogMessage :: LogMessage
simpleLogMessage = LogMessage (Just [("key", col)]) (Just 0)
  where col = ColumnValue (Just "some string value") Nothing Nothing Nothing
