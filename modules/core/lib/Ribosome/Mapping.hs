module Ribosome.Mapping where

import Ribosome.Control.Monad.Ribo (MonadRibo, NvimE, pluginName)
import Ribosome.Data.Mapping (Mapping(Mapping), MappingIdent(MappingIdent))
import Ribosome.Data.String (capitalize)
import Ribosome.Nvim.Api.Data (Buffer)
import Ribosome.Nvim.Api.IO (vimCommand)

activateMapping :: Mapping -> m ()
activateMapping _ =
  undefined

mapCommand :: String -> Bool -> String
mapCommand mode remap =
  mode ++ (if remap then "map" else "noremap")

activateBufferMapping ::
  MonadRibo m =>
  NvimE e m =>
  Buffer ->
  Mapping ->
  m ()
activateBufferMapping _ (Mapping (MappingIdent ident) lhs mode remap _) = do
  name <- pluginName
  vimCommand (unwords (cmdline name))
  where
    cmdline name = [cmd, "<buffer>", lhs, ":call", func name ++ "('" ++ ident ++ "')<cr>"]
    cmd = mapCommand mode remap
    func name = capitalize name ++ "Mapping"
