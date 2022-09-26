-- |Modify 'RpcCall' constructors that contain calls to @nvim_command@.
--
-- See @:h :command-modifiers@.
module Ribosome.Host.Modify where

import Data.MessagePack (Object (ObjectString))
import Exon (exon)

import Ribosome.Host.Api.Data (Buffer, Window)
import Ribosome.Host.Api.Data (
  nvimBufGetNumber,
  nvimGetCurrentBuf,
  nvimGetCurrentWin,
  nvimSetCurrentBuf,
  vimSetCurrentWindow,
  )
import Ribosome.Host.Data.Request (Request (Request))
import Ribosome.Host.Data.RpcCall (RpcCall (RpcRequest))
import qualified Ribosome.Host.Effect.Rpc as Rpc
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Class.MonadRpc (MonadRpc)

-- |Modify an 'RpcCall' constructor if it contains a request for @nvim_command@ by prefixing it with the given string.
modifyCall :: Text -> RpcCall a -> RpcCall a
modifyCall modifier = \case
  RpcRequest (Request "nvim_command" [ObjectString cmd]) decode ->
    RpcRequest (Request "nvim_command" [ObjectString [exon|#{encodeUtf8 modifier} #{cmd}|]]) decode
  c ->
    c

-- |Prefix all @nvim_commands@ called in an action with the given string.
modifyCmd ::
  Member Rpc r =>
  Text ->
  Sem r a ->
  Sem r a
modifyCmd modifier =
  interceptH \case
    Rpc.Sync call ->
      pureT =<< Rpc.sync (modifyCall modifier call)
    Rpc.Async call use ->
      pureT =<< Rpc.async (modifyCall modifier call) (\ r -> void (runTSimple (use r)))
    Rpc.Notify call ->
      pureT =<< Rpc.notify (modifyCall modifier call)
    Rpc.ChannelId ->
      pureT =<< Rpc.channelId

-- |Prefix all @nvim_commands@ called in an action with @silent@.
silent ::
  Member Rpc r =>
  Sem r a ->
  Sem r a
silent =
  modifyCmd "silent"

-- |Prefix all @nvim_commands@ called in an action with @silent!@.
silentBang ::
  Member Rpc r =>
  Sem r a ->
  Sem r a
silentBang =
  modifyCmd "silent!"

-- |Prefix all @nvim_commands@ called in an action with @noautocmd@.
noautocmd ::
  Member Rpc r =>
  Sem r a ->
  Sem r a
noautocmd =
  modifyCmd "noautocmd"

-- |Prefix all @nvim_commands@ called in an action with @windo N@ where @N@ is the number of the given window.
windo ::
  MonadRpc m =>
  Window ->
  m a ->
  m a
windo win ma = do
  previous <- nvimGetCurrentWin
  vimSetCurrentWindow win
  a <- ma
  when (previous /= win) (vimSetCurrentWindow previous)
  pure a

-- |Prefix all @nvim_commands@ called in an action with @bufdo N@ where @N@ is the number of the given buffer.
bufdo ::
  Member Rpc r =>
  Buffer ->
  Sem r a ->
  Sem r a
bufdo buf ma = do
  previous <- nvimGetCurrentBuf
  number <- nvimBufGetNumber buf
  a <- modifyCmd [exon|#{show number}bufdo|] ma
  when (previous /= buf) (nvimSetCurrentBuf previous)
  pure a
