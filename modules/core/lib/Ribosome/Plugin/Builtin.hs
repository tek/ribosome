module Ribosome.Plugin.Builtin where

import Neovim.Plugin.Classes (Synchronous(Sync))

import Data.MessagePack (Object(ObjectString, ObjectNil))
import Ribosome.Control.Monad.Ribo (MonadRibo, NvimE)
import Ribosome.Data.Mapping (MappingError)
import Ribosome.Data.Text (capitalize)
import Ribosome.Plugin.TH (RpcDef(RpcDef), RpcDefDetail(RpcFunction))
import Ribosome.Scratch (killScratch)

deleteScratch ::
  MonadRibo m =>
  NvimE e m =>
  [Object] ->
  m Object
deleteScratch [ObjectString name] =
  ObjectNil <$ killScratch (decodeUtf8 name)
deleteScratch _ =
  return ObjectNil

deleteScratchRpc ::
  MonadDeepError e MappingError m =>
  MonadRibo m =>
  NvimE e m =>
  Text ->
  RpcDef m
deleteScratchRpc pluginName =
  RpcDef (RpcFunction Sync) (capitalize pluginName <> "DeleteScratch") deleteScratch
