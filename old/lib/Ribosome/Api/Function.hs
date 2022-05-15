module Ribosome.Api.Function where

import qualified Data.Text as Text (intercalate)

import Ribosome.Host.Api.Effect (nvimExec)

defineFunction ::
  Member Rpc r =>
  Text ->
  [Text] ->
  [Text] ->
  m ()
defineFunction name params body =
  void $ nvimExec (unlines (sig : body ++ ["endfunction"])) True
  where
    sig =
      "function! " <> name <> "(" <> Text.intercalate ", " params <> ")"
