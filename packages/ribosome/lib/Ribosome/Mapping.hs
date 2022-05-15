module Ribosome.Mapping where

import Exon (exon)

import Ribosome.Data.Mapping (Mapping (Mapping), MappingIdent (MappingIdent))
import Ribosome.Data.PluginName (PluginName (PluginName))
import Ribosome.Host.Api.Data (Buffer)
import Ribosome.Host.Api.Effect (vimCommand)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Modify (bufdo)
import Ribosome.Text (capitalize)

mapCommand :: Text -> Bool -> Text
mapCommand mode remap =
  mode <> (if remap then "map" else "noremap")

mappingCall ::
  Text ->
  MappingIdent ->
  Text
mappingCall name (MappingIdent ident) =
  [exon|:silent call #{capitalize name}Mapping('#{ident}'<cr>)|]

mappingCmd ::
  Members [Rpc, Reader PluginName] r =>
  Text ->
  Mapping ->
  Sem r ()
mappingCmd extra (Mapping ident lhs mode remap _) = do
  PluginName name <- ask
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
