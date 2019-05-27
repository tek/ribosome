module Ribosome.Plugin.Builtin where

import Neovim.Plugin.Classes (Synchronous(Async))

import Data.MessagePack (Object(ObjectString, ObjectNil))
import Ribosome.Control.Monad.Ribo (MonadRibo, NvimE)
import Ribosome.Data.Mapping (MappingError)
import Ribosome.Data.Text (capitalize)
import Ribosome.Plugin.TH.Handler (RpcDef(RpcDef), RpcDefDetail(RpcFunction))
import Ribosome.Scratch (killScratchByName)

deleteScratch ::
  MonadRibo m =>
  NvimE e m =>
  [Object] ->
  m Object
deleteScratch [ObjectString name] =
  ObjectNil <$ killScratchByName (decodeUtf8 name)
deleteScratch _ =
  return ObjectNil

deleteScratchRpc ::
  MonadDeepError e MappingError m =>
  MonadRibo m =>
  NvimE e m =>
  Text ->
  RpcDef m
deleteScratchRpc pluginName =
  RpcDef (RpcFunction Async) (capitalize pluginName <> "DeleteScratch") deleteScratch
