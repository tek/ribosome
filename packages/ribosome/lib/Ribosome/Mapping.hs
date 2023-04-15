-- |Functions for constructing and activating 'Mapping's
module Ribosome.Mapping where

import Data.MessagePack (Object)
import Exon (exon)

import Ribosome.Data.Mapping (
  MapMode,
  Mapping (Mapping),
  MappingAction (MappingCall, MappingEvent),
  MappingId (MappingId),
  MappingLhs (MappingLhs),
  MappingSpec (MappingSpec),
  mapModeShortName,
  unMappingId,
  )
import Ribosome.Host.Api.Data (Buffer, nvimBufSetKeymap, nvimSetKeymap)
import Ribosome.Host.Class.MonadRpc (MonadRpc (atomic))
import Ribosome.Host.Data.ChannelId (ChannelId (ChannelId))
import Ribosome.Host.Data.Event (EventName (EventName))
import Ribosome.Host.Data.RpcHandler (RpcHandler (RpcHandler, rpcName))
import Ribosome.Host.Data.RpcName (RpcName (RpcName))
import qualified Ribosome.Host.Effect.Rpc as Rpc
import Ribosome.Host.Effect.Rpc (Rpc)

-- |Generate an atomic call executing a mapping cmd for all modes specified in the 'Mapping' and run it.
mappingCmdWith ::
  MonadRpc m =>
  (Text -> Text -> Text -> Map Text Object -> m ()) ->
  ChannelId ->
  Mapping ->
  m ()
mappingCmdWith call (ChannelId cid) (Mapping action (MappingSpec (MappingLhs lhs) modes) ident opts) =
  for_ modes \ mode ->
    call (mapModeShortName mode) lhs [exon|<cmd>#{cmd}<cr>|] opts
  where
    cmd =
      command action
    command = \case
      MappingCall (RpcName name) ->
        [exon|silent #{name}#{i}|]
      MappingEvent (EventName name) ->
        [exon|call rpcnotify(#{show cid}, '#{name}'#{foldMap idArg ident})|]
    i =
      foldMap (.unMappingId) ident
    idArg = \case
      MappingId mi -> [exon|, '#{mi}'|]

-- |Generate an atomic call executing a mapping cmd for all modes specified in the 'Mapping' and run it.
mappingCmd ::
  MonadRpc m =>
  ChannelId ->
  Mapping ->
  m ()
mappingCmd = do
  mappingCmdWith nvimSetKeymap

-- |Generate an atomic call executing a buffer mapping cmd for all modes specified in the 'Mapping' and run it.
bufferMappingCmd ::
  MonadRpc m =>
  -- |Use @<buffer>@ to create a buffer-local mapping.
  Buffer ->
  ChannelId ->
  Mapping ->
  m ()
bufferMappingCmd buffer =
  mappingCmdWith (nvimBufSetKeymap buffer)

-- |Register a mapping globally.
activateMappingOnChan ::
  MonadRpc m =>
  ChannelId ->
  Mapping ->
  m ()
activateMappingOnChan =
  mappingCmd

-- |Register a mapping in the supplied buffer.
activateBufferMappingOnChan ::
  MonadRpc m =>
  Buffer ->
  ChannelId ->
  Mapping ->
  m ()
activateBufferMappingOnChan buffer =
  bufferMappingCmd buffer

-- |Register a mapping globally.
--
-- This obtains the channel ID and cannot be batched atomically.
activateMapping ::
  Member Rpc r =>
  Mapping ->
  Sem r ()
activateMapping m = do
  cid <- Rpc.channelId
  mappingCmd cid m

-- |Register a mapping in the supplied buffer.
--
-- This obtains the channel ID and cannot be batched atomically.
activateBufferMapping ::
  Member Rpc r =>
  Buffer ->
  Mapping ->
  Sem r ()
activateBufferMapping buffer m = do
  cid <- Rpc.channelId
  bufferMappingCmd buffer cid m

-- |Register a set of mappings globally.
--
-- This obtains the channel ID and cannot be batched atomically.
activateMappings ::
  Member Rpc r =>
  [Mapping] ->
  Sem r ()
activateMappings m = do
  cid <- Rpc.channelId
  traverse_ (mappingCmd cid) m

-- |Register a set of mappings in the supplied buffer.
--
-- This obtains the channel ID and cannot be batched atomically.
activateBufferMappings ::
  Member Rpc r =>
  Buffer ->
  [Mapping] ->
  Sem r ()
activateBufferMappings buffer m = do
  cid <- Rpc.channelId
  atomic do
    traverse_ (bufferMappingCmd buffer cid) m

-- |Construct a 'Mapping' using the name from the supplied 'RpcHandler'.
mappingFor ::
  RpcHandler r ->
  MappingLhs ->
  NonEmpty MapMode ->
  Maybe MappingId ->
  Map Text Object ->
  Mapping
mappingFor RpcHandler {rpcName} lhs mode =
  Mapping (MappingCall rpcName) (MappingSpec lhs mode)

-- |Construct a 'Mapping' using the supplied 'EventName'.
eventMapping ::
  EventName ->
  MappingLhs ->
  NonEmpty MapMode ->
  Maybe MappingId ->
  Map Text Object ->
  Mapping
eventMapping event lhs mode =
  Mapping (MappingEvent event) (MappingSpec lhs mode)
