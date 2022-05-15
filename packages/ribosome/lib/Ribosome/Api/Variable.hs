module Ribosome.Api.Variable where

import Exon (exon)

import Ribosome.Data.PluginName (PluginName (PluginName))
import Ribosome.Host.Api.Effect (nvimSetVar)
import Ribosome.Host.Class.Msgpack.Encode (MsgpackEncode)
import Ribosome.Host.Effect.Rpc (Rpc)

setPVar ::
  Members [Rpc, Reader PluginName] r =>
  MsgpackEncode a =>
  Text ->
  a ->
  Sem r ()
setPVar var a = do
  PluginName name <- ask
  nvimSetVar [exon|#{name}_#{var}|] a
