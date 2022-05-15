module Ribosome.Plugin.Builtin where

import Data.MessagePack (Object(ObjectString, ObjectNil))
import Neovim.Plugin.Classes (Synchronous(Async))

import Ribosome.Control.Monad.Ribo (MonadRibo, NvimE)
import Ribosome.Data.Text (capitalize)
import Ribosome.Plugin.TH.Handler (RpcDef(RpcDef), RpcDefDetail(RpcFunction))
import Ribosome.Scratch (killScratchByName)

deleteScratch ::
  MonadRibo m =>
  NvimE e m =>
  [Object] ->
  m Object
deleteScratch [ObjectString name] = do
  ObjectNil <$ killScratchByName (decodeUtf8 name)
deleteScratch _ =
  return ObjectNil

deleteScratchRpc ::
  MonadRibo m =>
  NvimE e m =>
  Text ->
  RpcDef m
deleteScratchRpc pluginName =
  RpcDef (RpcFunction Async) (capitalize pluginName <> "DeleteScratch") deleteScratch
