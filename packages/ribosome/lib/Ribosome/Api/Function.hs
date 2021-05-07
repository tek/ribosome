module Ribosome.Api.Function where

import qualified Data.Text as Text (intercalate)

import Ribosome.Control.Monad.Ribo (NvimE)
import Ribosome.Nvim.Api.IO (vimCommand)

defineFunction ::
  NvimE e m =>
  Text ->
  [Text] ->
  [Text] ->
  m ()
defineFunction name params body =
  vimCommand $ unlines $ sig : body ++ ["endfunction"]
  where
    sig =
      "function! " <> name <> "(" <> Text.intercalate ", " params <> ")"
