module Ribosome.Host.Modify where

import Data.MessagePack (Object (ObjectString))
import Exon (exon)

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
