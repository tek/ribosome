module Ribosome.Mapping where

import Ribosome.Control.Monad.Ribo (MonadRibo, NvimE, pluginName)
import Ribosome.Data.Mapping (Mapping(Mapping), MappingIdent(MappingIdent))
import Ribosome.Data.Text (capitalize)
import Ribosome.Nvim.Api.Data (Buffer)
import Ribosome.Nvim.Api.IO (bufferGetNumber, vimCommand, vimGetCurrentBuffer, vimSetCurrentBuffer)

activateMapping :: Mapping -> m ()
activateMapping _ =
  undefined

mapCommand :: Text -> Bool -> Text
mapCommand mode remap =
  mode <> (if remap then "map" else "noremap")

activateBufferMapping ::
  MonadRibo m =>
  NvimE e m =>
  Buffer ->
  Mapping ->
  m ()
activateBufferMapping buffer (Mapping (MappingIdent ident) lhs mode remap _) = do
  previous <- vimGetCurrentBuffer
  name <- pluginName
  number <- bufferGetNumber buffer
  vimCommand (unwords (cmdline name number))
  when (buffer /= previous) (vimSetCurrentBuffer previous)
  where
    cmdline name number =
      [show number, "bufdo ", cmd, "<buffer>", lhs, ":silent call", func name <> "('" <> ident <> "')<cr>"]
    cmd = mapCommand mode remap
    func name = capitalize name <> "Mapping"
