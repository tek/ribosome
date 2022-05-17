module Ribosome.Api.Echo where

import Exon (exon)

import Ribosome.Data.PluginName (PluginName (PluginName))
import Ribosome.Host.Api.Effect (nvimEcho)
import Ribosome.Host.Class.Msgpack.Array (msgpackArray)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.PluginName (pluginName)

simpleEcho ::
  Member Rpc r =>
  Bool ->
  Text ->
  Sem r ()
simpleEcho history msg =
  nvimEcho [msgpackArray msg] history mempty

prefixedEcho ::
  Members [Rpc, Reader PluginName] r =>
  Bool ->
  Text ->
  Sem r ()
prefixedEcho history msg = do
  PluginName name <- pluginName
  simpleEcho history [exon|#{name}: #{msg}|]

echohl ::
  Member Rpc r =>
  Bool ->
  Text ->
  Text ->
  Sem r ()
echohl history hl msg =
  nvimEcho [msgpackArray msg hl] history mempty

echo ::
  Members [Rpc, Reader PluginName] r =>
  Text ->
  Sem r ()
echo =
  prefixedEcho False

echom ::
  Members [Rpc, Reader PluginName] r =>
  Text ->
  Sem r ()
echom =
  prefixedEcho True
