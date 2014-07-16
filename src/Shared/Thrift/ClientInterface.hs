module Shared.Thrift.ClientInterface (
  Server(..),
  sendLeafLog,
  sendLeafQuery,
  sendIngestorLog,
  sendInternalAggregatorQuery
) where

import Shared.Thrift.Types
import Shared.Thrift.Interface

import Thrift
import Thrift.Protocol.Binary
import Thrift.Transport.Handle
import Network
import GHC.IO.Handle.Types (Handle())

import qualified LeafNodeService_Client as LeafNodeClient
import qualified IngestorService_Client as IngestorClient
import qualified InternalAggregatorService_Client as InternalAggregatorClient


import Data.Vector (Vector())
import Control.Applicative ((<$>))

data Server = Server HostName PortNumber
  deriving (Eq, Show)

type GhcTransport = GHC.IO.Handle.Types.Handle

protocol :: (Transport t) => t -> BinaryProtocol t
protocol = BinaryProtocol

-- send :: (Protocol p, TypeEquiv hReq tReq, TypeEquiv hResp tResp) =>
--      (GhcTransport -> p GhcTransport)
--   -> Server
--   -> ((p GhcTransport, p GhcTransport) -> tReq -> IO tResp)
--   -> hReq
--   -> IO (Maybe hResp)
-- send proto (Server name port) iface req = do
--   transport <- hOpen (name, PortNumber port)
--   let protos = (proto transport, proto transport)
--   resp <- iface protos (toThrift req)
--   tClose transport
--   return $ fromThrift resp

send :: (Protocol p) =>
     (GhcTransport -> p GhcTransport)
  -> Server
  -> ((p GhcTransport, p GhcTransport) -> IO tResp)
  -> IO tResp
send proto (Server name port) iface = do
  transport <- hOpen (name, PortNumber port)
  let protos = (proto transport, proto transport)
  resp <- iface protos
  tClose transport
  return resp

--------------------

sendLeafLog :: Server -> Vector LogMessage -> IO (Maybe LogResponse)
sendLeafLog server message = fromThrift <$> send protocol server (`LeafNodeClient.log` toThrift message)

sendLeafQuery :: Server -> Query -> IO (Maybe QueryResponse)
sendLeafQuery server query = fromThrift <$> send protocol server (`LeafNodeClient.query` toThrift query)


sendIngestorLog :: Server -> Vector LogMessage -> IO (Maybe LogResponse)
sendIngestorLog server message = fromThrift <$> send protocol server (`IngestorClient.log` toThrift message)


sendInternalAggregatorQuery :: Server -> Query -> Vector ServerID -> IO (Maybe QueryResponse)
sendInternalAggregatorQuery server query serverIDs = fromThrift <$> send protocol server (\p -> InternalAggregatorClient.queryInternal p (toThrift query) serverIDs)
