-- |API functions for echoing messages in Neovim.
module Ribosome.Api.Echo where

import Ribosome.Data.PluginName (PluginName)
import Ribosome.Host.Api.Effect (nvimEcho)
import Ribosome.Host.Class.Msgpack.Array (msgpackArray)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.PluginName (pluginNamePrefixed)

-- |Echo a string, adding it to the message history if the first argument is 'True'.
simpleEcho ::
  Member Rpc r =>
  Bool ->
  Text ->
  Sem r ()
simpleEcho history msg =
  nvimEcho [msgpackArray msg] history mempty

-- |Echo a string prefixed with the plugin name, adding it to the message history if the first argument is 'True'.
prefixedEcho ::
  Members [Rpc, Reader PluginName] r =>
  Bool ->
  Text ->
  Sem r ()
prefixedEcho history msg = do
  pref <- pluginNamePrefixed msg
  simpleEcho history pref

-- |Echo a string with a highlight group.
echohl ::
  Member Rpc r =>
  Bool ->
  Text ->
  Text ->
  Sem r ()
echohl history hl msg =
  nvimEcho [msgpackArray msg hl] history mempty

-- |Echo a string prefixed with the plugin name, without adding it to the message history.
echo ::
  Members [Rpc, Reader PluginName] r =>
  Text ->
  Sem r ()
echo =
  prefixedEcho False

-- |Echo a string prefixed with the plugin name and add it to the message history.
echom ::
  Members [Rpc, Reader PluginName] r =>
  Text ->
  Sem r ()
echom =
  prefixedEcho True
