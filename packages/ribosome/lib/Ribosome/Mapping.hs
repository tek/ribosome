module Ribosome.Mapping where

import qualified Data.Map.Strict as Map
import Exon (exon)

import Ribosome.Data.Mapping (Mapping (Mapping), MappingIdent (MappingIdent))
import Ribosome.Data.PluginName (PluginName (PluginName))
import Ribosome.Host.Api.Data (Buffer)
import Ribosome.Host.Api.Effect (vimCommand)
import Ribosome.Host.Data.HandlerError (HandlerError)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Modify (bufdo)
import Ribosome.PluginName (pluginNameCapitalized)

mapCommand :: Text -> Bool -> Text
mapCommand mode remap =
  mode <> (if remap then "map" else "noremap")

mappingCall ::
  Text ->
  MappingIdent ->
  Text
mappingCall name (MappingIdent ident) =
  [exon|:silent call #{name}Mapping('#{ident}')<cr>|]

mappingCmd ::
  Members [Rpc, Reader PluginName] r =>
  Text ->
  Mapping ->
  Sem r ()
mappingCmd extra (Mapping ident lhs mode remap _) = do
  PluginName name <- pluginNameCapitalized
  vimCommand (cmdline name)
  where
    cmdline name =
      [exon|#{mapCommand mode remap}#{extra} #{lhs} #{mappingCall name ident}|]

activateMapping ::
  Members [Rpc, Reader PluginName] r =>
  Mapping ->
  Sem r ()
activateMapping =
  mappingCmd ""

activateBufferMapping ::
  Members [Rpc, Reader PluginName] r =>
  Buffer ->
  Mapping ->
  Sem r ()
activateBufferMapping buffer mapping =
  bufdo buffer do
    mappingCmd " <buffer>" mapping

mappingHandler ::
  Map MappingIdent (Sem (Stop HandlerError : r) ()) ->
  MappingIdent ->
  Sem (Stop HandlerError : r) ()
mappingHandler maps i =
  join (stopNote "No handler for this mapping" (Map.lookup i maps))
