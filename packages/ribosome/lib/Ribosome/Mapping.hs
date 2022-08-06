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
import qualified Ribosome.Host.Api.Data as Data
import Ribosome.Host.Api.Data (Buffer)
import Ribosome.Host.Data.ChannelId (ChannelId (ChannelId))
import Ribosome.Host.Data.Event (EventName (EventName))
import Ribosome.Host.Data.RpcCall (RpcCall)
import Ribosome.Host.Data.RpcHandler (RpcHandler (RpcHandler, rpcName))
import Ribosome.Host.Data.RpcName (RpcName (RpcName))
import qualified Ribosome.Host.Effect.Rpc as Rpc
import Ribosome.Host.Effect.Rpc (Rpc)

-- |Generate an atomic call executing a mapping cmd for all modes specified in the 'Mapping' and run it.
mappingCmdWith ::
  Member Rpc r =>
  (Text -> Text -> Text -> Map Text Object -> RpcCall ()) ->
  Mapping ->
  Sem r ()
mappingCmdWith call (Mapping action (MappingSpec (MappingLhs lhs) modes) ident opts) = do
  cmd <- command action
  Rpc.sync $ for_ modes \ mode ->
    call (mapModeShortName mode) lhs [exon|<cmd>#{cmd}<cr>|] opts
  where
    command = \case
      MappingCall (RpcName name) ->
        pure [exon|silent #{name}#{i}|]
      MappingEvent (EventName name) -> do
        ChannelId cid <- Rpc.channelId
        pure [exon|call rpcnotify(#{show cid}, '#{name}'#{foldMap idArg ident})|]
    i =
      foldMap unMappingId ident
    idArg = \case
      MappingId mi -> [exon|, '#{mi}'|]

-- |Generate an atomic call executing a mapping cmd for all modes specified in the 'Mapping' and run it.
mappingCmd ::
  Member Rpc r =>
  Mapping ->
  Sem r ()
mappingCmd = do
  mappingCmdWith Data.nvimSetKeymap

-- |Generate an atomic call executing a buffer mapping cmd for all modes specified in the 'Mapping' and run it.
bufferMappingCmd ::
  Member Rpc r =>
  -- |Use @<buffer>@ to create a buffer-local mapping.
  Buffer ->
  Mapping ->
  Sem r ()
bufferMappingCmd buffer =
  mappingCmdWith (Data.nvimBufSetKeymap buffer)

-- |Register a mapping globally.
activateMapping ::
  Member Rpc r =>
  Mapping ->
  Sem r ()
activateMapping =
  mappingCmd

-- |Register a mapping in the supplied buffer.
activateBufferMapping ::
  Member Rpc r =>
  Buffer ->
  Mapping ->
  Sem r ()
activateBufferMapping buffer =
  bufferMappingCmd buffer

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
