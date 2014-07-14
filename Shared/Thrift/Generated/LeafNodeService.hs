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

module LeafNodeService where
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


import Huba_Types
import qualified LeafNodeService_Iface as Iface
-- HELPER FUNCTIONS AND STRUCTURES --

data Log_args = Log_args{f_Log_args_logBatch :: Maybe (Vector.Vector LogMessage)} deriving (Show,Eq,Typeable)
instance Hashable Log_args where
  hashWithSalt salt record = salt   `hashWithSalt` f_Log_args_logBatch record  
write_Log_args oprot record = do
  writeStructBegin oprot "Log_args"
  case f_Log_args_logBatch record of {Nothing -> return (); Just _v -> do
    writeFieldBegin oprot ("logBatch",T_LIST,1)
    (let f = Vector.mapM_ (\_viter105 -> write_LogMessage oprot _viter105) in do {writeListBegin oprot (T_STRUCT,fromIntegral $ Vector.length _v); f _v;writeListEnd oprot})
    writeFieldEnd oprot}
  writeFieldStop oprot
  writeStructEnd oprot
read_Log_args_fields iprot record = do
  (_,_t107,_id108) <- readFieldBegin iprot
  if _t107 == T_STOP then return record else
    case _id108 of 
      1 -> if _t107 == T_LIST then do
        s <- (let f n = Vector.replicateM (fromIntegral n) ((read_LogMessage iprot)) in do {(_etype112,_size109) <- readListBegin iprot; f _size109})
        read_Log_args_fields iprot record{f_Log_args_logBatch=Just s}
        else do
          skip iprot _t107
          read_Log_args_fields iprot record
      _ -> do
        skip iprot _t107
        readFieldEnd iprot
        read_Log_args_fields iprot record
read_Log_args iprot = do
  _ <- readStructBegin iprot
  record <- read_Log_args_fields iprot (Log_args{f_Log_args_logBatch=Nothing})
  readStructEnd iprot
  return record
data Log_result = Log_result{f_Log_result_success :: Maybe LogResponse} deriving (Show,Eq,Typeable)
instance Hashable Log_result where
  hashWithSalt salt record = salt   `hashWithSalt` f_Log_result_success record  
write_Log_result oprot record = do
  writeStructBegin oprot "Log_result"
  case f_Log_result_success record of {Nothing -> return (); Just _v -> do
    writeFieldBegin oprot ("success",T_STRUCT,0)
    write_LogResponse oprot _v
    writeFieldEnd oprot}
  writeFieldStop oprot
  writeStructEnd oprot
read_Log_result_fields iprot record = do
  (_,_t117,_id118) <- readFieldBegin iprot
  if _t117 == T_STOP then return record else
    case _id118 of 
      0 -> if _t117 == T_STRUCT then do
        s <- (read_LogResponse iprot)
        read_Log_result_fields iprot record{f_Log_result_success=Just s}
        else do
          skip iprot _t117
          read_Log_result_fields iprot record
      _ -> do
        skip iprot _t117
        readFieldEnd iprot
        read_Log_result_fields iprot record
read_Log_result iprot = do
  _ <- readStructBegin iprot
  record <- read_Log_result_fields iprot (Log_result{f_Log_result_success=Nothing})
  readStructEnd iprot
  return record
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
  (_,_t122,_id123) <- readFieldBegin iprot
  if _t122 == T_STOP then return record else
    case _id123 of 
      1 -> if _t122 == T_STRUCT then do
        s <- (read_Query iprot)
        read_Query_args_fields iprot record{f_Query_args_query=Just s}
        else do
          skip iprot _t122
          read_Query_args_fields iprot record
      _ -> do
        skip iprot _t122
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
  (_,_t127,_id128) <- readFieldBegin iprot
  if _t127 == T_STOP then return record else
    case _id128 of 
      0 -> if _t127 == T_STRUCT then do
        s <- (read_QueryResponse iprot)
        read_Query_result_fields iprot record{f_Query_result_success=Just s}
        else do
          skip iprot _t127
          read_Query_result_fields iprot record
      _ -> do
        skip iprot _t127
        readFieldEnd iprot
        read_Query_result_fields iprot record
read_Query_result iprot = do
  _ <- readStructBegin iprot
  record <- read_Query_result_fields iprot (Query_result{f_Query_result_success=Nothing})
  readStructEnd iprot
  return record
process_log (seqid, iprot, oprot, handler) = do
  args <- read_Log_args iprot
  readMessageEnd iprot
  rs <- return (Log_result Nothing)
  res <- (do
    res <- Iface.log handler (f_Log_args_logBatch args)
    return rs{f_Log_result_success= Just res})
  writeMessageBegin oprot ("log", M_REPLY, seqid);
  write_Log_result oprot res
  writeMessageEnd oprot
  tFlush (getTransport oprot)
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
  "log" -> process_log (seqid,iprot,oprot,handler)
  "query" -> process_query (seqid,iprot,oprot,handler)
  _ -> do
    skip iprot T_STRUCT
    readMessageEnd iprot
    writeMessageBegin oprot (name,M_EXCEPTION,seqid)
    writeAppExn oprot (AppExn AE_UNKNOWN_METHOD ("Unknown function " ++ TL.unpack name))
    writeMessageEnd oprot
    tFlush (getTransport oprot)
process handler (iprot, oprot) = do
  (name, typ, seqid) <- readMessageBegin iprot
  proc_ handler (iprot,oprot) (name,typ,seqid)
  return True
