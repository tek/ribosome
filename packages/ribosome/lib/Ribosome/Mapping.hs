-- |Functions for constructing and activating 'Mapping's
module Ribosome.Mapping where

import Exon (exon)

import Ribosome.Data.Mapping (MapMode, Mapping (Mapping), MappingId, noremapCmd, unMappingId)
import qualified Ribosome.Host.Api.Data as Data
import Ribosome.Host.Api.Data (Buffer)
import Ribosome.Host.Data.RpcHandler (RpcHandler (RpcHandler, rpcName))
import Ribosome.Host.Data.RpcName (RpcName (RpcName))
import qualified Ribosome.Host.Effect.Rpc as Rpc
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Modify (bufdo)

-- |Generate an atomic call executing a mapping cmd for all modes specified in the 'Mapping'.
mappingCmd ::
  Member Rpc r =>
  -- |Use @<buffer>@ to create a buffer-local mapping.
  Bool ->
  Mapping ->
  Sem r ()
mappingCmd buffer (Mapping (RpcName name) lhs modes ident) =
  Rpc.sync $ for_ modes \ mode ->
    Data.nvimCommand [exon|#{noremapCmd mode}#{buf} #{lhs} <cmd>silent #{name}#{i}<cr>|]
  where
    buf =
      if buffer then " <buffer>" else ""
    i =
      foldMap unMappingId ident

-- |Register a mapping globally.
activateMapping ::
  Member Rpc r =>
  Mapping ->
  Sem r ()
activateMapping =
  mappingCmd False

-- |Register a mapping in the supplied buffer.
activateBufferMapping ::
  Member Rpc r =>
  Buffer ->
  Mapping ->
  Sem r ()
activateBufferMapping buffer m =
  bufdo buffer do
    mappingCmd True m

-- |Construct a 'Mapping' using the name from the supplied 'RpcHandler'.
mappingFor ::
  RpcHandler r ->
  Text ->
  NonEmpty MapMode ->
  Maybe MappingId ->
  Mapping
mappingFor RpcHandler {rpcName} =
  Mapping rpcName
