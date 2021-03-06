{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-----------------------------------------------------------------
-- Autogenerated by Thrift Compiler (0.9.1)                      --
--                                                             --
-- DO NOT EDIT UNLESS YOU ARE SURE YOU KNOW WHAT YOU ARE DOING --
-----------------------------------------------------------------

module AggregatorService where
import Prelude ( Bool(..), Enum, Double, String, Maybe(..),
                 Eq, Show, Ord,
                 return, length, IO, fromIntegral, fromEnum, toEnum,
                 (.), (&&), (||), (==), (++), ($), (-) )

import Control.Exception
import Data.ByteString.Lazy
import Data.Hashable
import Data.Int
import Data.Text.Lazy ( Text )
import qualified Data.Text.Lazy as TL
import Data.Typeable ( Typeable )
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import qualified Data.Vector as Vector

import Thrift
import Thrift.Types ()


import qualified CommonService
import Huba_Types
import qualified AggregatorService_Iface as Iface
-- HELPER FUNCTIONS AND STRUCTURES --

data Query_args = Query_args{f_Query_args_query :: Maybe Query} deriving (Show,Eq,Typeable)
instance Hashable Query_args where
  hashWithSalt salt record = salt   `hashWithSalt` f_Query_args_query record  
write_Query_args oprot record = do
  writeStructBegin oprot "Query_args"
  case f_Query_args_query record of {Nothing -> return (); Just _v -> do
    writeFieldBegin oprot ("query",T_STRUCT,1)
    write_Query oprot _v
    writeFieldEnd oprot}
  writeFieldStop oprot
  writeStructEnd oprot
read_Query_args_fields iprot record = do
  (_,_t140,_id141) <- readFieldBegin iprot
  if _t140 == T_STOP then return record else
    case _id141 of 
      1 -> if _t140 == T_STRUCT then do
        s <- (read_Query iprot)
        read_Query_args_fields iprot record{f_Query_args_query=Just s}
        else do
          skip iprot _t140
          read_Query_args_fields iprot record
      _ -> do
        skip iprot _t140
        readFieldEnd iprot
        read_Query_args_fields iprot record
read_Query_args iprot = do
  _ <- readStructBegin iprot
  record <- read_Query_args_fields iprot (Query_args{f_Query_args_query=Nothing})
  readStructEnd iprot
  return record
data Query_result = Query_result{f_Query_result_success :: Maybe QueryResponse} deriving (Show,Eq,Typeable)
instance Hashable Query_result where
  hashWithSalt salt record = salt   `hashWithSalt` f_Query_result_success record  
write_Query_result oprot record = do
  writeStructBegin oprot "Query_result"
  case f_Query_result_success record of {Nothing -> return (); Just _v -> do
    writeFieldBegin oprot ("success",T_STRUCT,0)
    write_QueryResponse oprot _v
    writeFieldEnd oprot}
  writeFieldStop oprot
  writeStructEnd oprot
read_Query_result_fields iprot record = do
  (_,_t145,_id146) <- readFieldBegin iprot
  if _t145 == T_STOP then return record else
    case _id146 of 
      0 -> if _t145 == T_STRUCT then do
        s <- (read_QueryResponse iprot)
        read_Query_result_fields iprot record{f_Query_result_success=Just s}
        else do
          skip iprot _t145
          read_Query_result_fields iprot record
      _ -> do
        skip iprot _t145
        readFieldEnd iprot
        read_Query_result_fields iprot record
read_Query_result iprot = do
  _ <- readStructBegin iprot
  record <- read_Query_result_fields iprot (Query_result{f_Query_result_success=Nothing})
  readStructEnd iprot
  return record
process_query (seqid, iprot, oprot, handler) = do
  args <- read_Query_args iprot
  readMessageEnd iprot
  rs <- return (Query_result Nothing)
  res <- (do
    res <- Iface.query handler (f_Query_args_query args)
    return rs{f_Query_result_success= Just res})
  writeMessageBegin oprot ("query", M_REPLY, seqid);
  write_Query_result oprot res
  writeMessageEnd oprot
  tFlush (getTransport oprot)
proc_ handler (iprot,oprot) (name,typ,seqid) = case name of
  "query" -> process_query (seqid,iprot,oprot,handler)
  _ -> CommonService.proc_ handler (iprot,oprot) (name,typ,seqid)
process handler (iprot, oprot) = do
  (name, typ, seqid) <- readMessageBegin iprot
  proc_ handler (iprot,oprot) (name,typ,seqid)
  return True
