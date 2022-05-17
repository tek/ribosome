module Ribosome.Api.Function where

import qualified Data.Text as Text
import Exon (exon)

import Ribosome.Host.Api.Effect (nvimExec)
import Ribosome.Host.Effect.Rpc (Rpc)

defineFunction ::
  Member Rpc r =>
  Text ->
  [Text] ->
  [Text] ->
  Sem r ()
defineFunction name params body =
  void $ nvimExec (unlines (sig : body ++ ["endfunction"])) True
  where
    sig =
      [exon|function! #{name}(#{Text.intercalate ", " params})|]
