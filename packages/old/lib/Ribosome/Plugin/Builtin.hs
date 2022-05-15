module Ribosome.Plugin.Builtin where

import Data.MessagePack (Object(ObjectString, ObjectNil))
import Neovim.Plugin.Classes (Synchronous(Async))

import Ribosome.Data.Text (capitalize)
import Ribosome.Plugin.TH.Handler (RpcDef(RpcDef), RpcDefDetail(RpcFunction))
import Ribosome.Scratch (killScratchByName)

deleteScratch ::
  Member Rpc r =>
  [Object] ->
  m Object
deleteScratch [ObjectString name] = do
  ObjectNil <$ killScratchByName (decodeUtf8 name)
deleteScratch _ =
  pure ObjectNil

deleteScratchRpc ::
  Member Rpc r =>
  Text ->
  RpcDef m
deleteScratchRpc pluginName =
  RpcDef (RpcFunction Async) (capitalize pluginName <> "DeleteScratch") deleteScratch
