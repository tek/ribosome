module Ribosome.Mapping where

import Exon (exon)

import Ribosome.Data.Mapping (Mapping (Mapping), MappingId, unMappingId)
import Ribosome.Host.Api.Data (Buffer)
import Ribosome.Host.Api.Effect (nvimCommand)
import Ribosome.Host.Data.RpcHandler (RpcHandler (RpcHandler, rpcName))
import Ribosome.Host.Data.RpcName (RpcName (RpcName))
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Modify (bufdo)

mappingCmd ::
  Member Rpc r =>
  Bool ->
  Mapping ->
  Sem r ()
mappingCmd buffer (Mapping (RpcName name) lhs mode ident) =
  nvimCommand [exon|#{mode}noremap#{buf} #{lhs} <cmd>silent #{name}#{i}<cr>|]
  where
    buf =
      if buffer then " <buffer>" else ""
    i =
      foldMap unMappingId ident

activateMapping ::
  Member Rpc r =>
  Mapping ->
  Sem r ()
activateMapping =
  mappingCmd False

activateBufferMapping ::
  Member Rpc r =>
  Buffer ->
  Mapping ->
  Sem r ()
activateBufferMapping buffer m =
  bufdo buffer do
    mappingCmd True m

mappingFor ::
  RpcHandler r ->
  Text ->
  Text ->
  Maybe MappingId ->
  Mapping
mappingFor RpcHandler {rpcName} =
  Mapping rpcName
