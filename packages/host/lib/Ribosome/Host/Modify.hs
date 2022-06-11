module Ribosome.Host.Modify where

import Data.MessagePack (Object (ObjectString))
import Exon (exon)

import Ribosome.Host.Api.Data (Buffer, Window)
import Ribosome.Host.Api.Effect (
  nvimBufGetNumber,
  nvimGetCurrentBuf,
  nvimGetCurrentWin,
  nvimSetCurrentBuf,
  nvimWinGetNumber,
  vimSetCurrentWindow,
  )
import Ribosome.Host.Data.Request (Request (Request))
import Ribosome.Host.Data.RpcCall (RpcCall (RpcCallRequest))
import qualified Ribosome.Host.Effect.Rpc as Rpc
import Ribosome.Host.Effect.Rpc (Rpc)

modifyCall :: Text -> RpcCall a -> RpcCall a
modifyCall modifier = \case
  RpcCallRequest (Request "nvim_command" [ObjectString cmd]) ->
    RpcCallRequest (Request "nvim_command" [ObjectString [exon|#{encodeUtf8 modifier} #{cmd}|]])
  c ->
    c

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

silent ::
  Member Rpc r =>
  Sem r a ->
  Sem r a
silent =
  modifyCmd "silent"

silentBang ::
  Member Rpc r =>
  Sem r a ->
  Sem r a
silentBang =
  modifyCmd "silent!"

noautocmd ::
  Member Rpc r =>
  Sem r a ->
  Sem r a
noautocmd =
  modifyCmd "noautocmd"

windo ::
  Member Rpc r =>
  Window ->
  Sem r a ->
  Sem r a
windo win ma = do
  previous <- nvimGetCurrentWin
  number <- nvimWinGetNumber win
  a <- modifyCmd [exon|#{show number}windo|] ma
  when (previous /= win) (vimSetCurrentWindow previous)
  pure a

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
