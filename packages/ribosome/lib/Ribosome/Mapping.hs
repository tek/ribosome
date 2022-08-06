-- |Functions for constructing and activating 'Mapping's
module Ribosome.Mapping where

import Exon (exon)

import Ribosome.Data.Mapping (
  MapMode,
  Mapping (Mapping),
  MappingAction (MappingCall, MappingEvent),
  MappingId (MappingId),
  MappingLhs (MappingLhs),
  MappingSpec (MappingSpec),
  noremapCmd,
  unMappingId,
  )
import qualified Ribosome.Host.Api.Data as Data
import Ribosome.Host.Api.Data (Buffer)
import Ribosome.Host.Data.ChannelId (ChannelId (ChannelId))
import Ribosome.Host.Data.Event (EventName (EventName))
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
mappingCmd buffer (Mapping action (MappingSpec (MappingLhs lhs) modes) ident) = do
  cmd <- command action
  Rpc.sync $ for_ modes \ mode ->
    Data.nvimCommand [exon|#{noremapCmd mode}#{buf} #{lhs} <cmd>#{cmd}<cr>|]
  where
    command = \case
      MappingCall (RpcName name) ->
        pure [exon|silent #{name}#{i}|]
      MappingEvent (EventName name) -> do
        ChannelId cid <- Rpc.channelId
        pure [exon|call rpcnotify(#{show cid}, '#{name}'#{foldMap idArg ident})|]
    buf =
      if buffer then " <buffer>" else ""
    i =
      foldMap unMappingId ident
    idArg = \case
      MappingId mi -> [exon|, '#{mi}'|]

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
  MappingLhs ->
  NonEmpty MapMode ->
  Maybe MappingId ->
  Mapping
mappingFor RpcHandler {rpcName} lhs mode =
  Mapping (MappingCall rpcName) (MappingSpec lhs mode)

-- |Construct a 'Mapping' using the supplied 'EventName'.
eventMapping ::
  EventName ->
  MappingLhs ->
  NonEmpty MapMode ->
  Maybe MappingId ->
  Mapping
eventMapping event lhs mode =
  Mapping (MappingEvent event) (MappingSpec lhs mode)
