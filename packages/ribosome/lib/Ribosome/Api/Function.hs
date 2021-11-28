module Ribosome.Api.Function where

import qualified Data.Text as Text (intercalate)

import Ribosome.Control.Monad.Ribo (NvimE)
import Ribosome.Nvim.Api.IO (nvimExec)

defineFunction ::
  NvimE e m =>
  Text ->
  [Text] ->
  [Text] ->
  m ()
defineFunction name params body =
  void $ nvimExec (unlines (sig : body ++ ["endfunction"])) True
  where
    sig =
      "function! " <> name <> "(" <> Text.intercalate ", " params <> ")"
