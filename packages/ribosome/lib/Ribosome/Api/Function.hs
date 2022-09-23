-- |Defining Neovim functions.
module Ribosome.Api.Function where

import qualified Data.Text as Text
import Exon (exon)

import Ribosome.Host.Api.Data (nvimExec)
import Ribosome.Host.Effect.Rpc (Rpc)

-- |Define a Neovim function.
defineFunction ::
  Member Rpc r =>
  -- |Function name.
  Text ->
  -- |Function parameters.
  [Text] ->
  -- |Vimscript lines that form the function body.
  [Text] ->
  Sem r ()
defineFunction name params body =
  void $ nvimExec (unlines (sig : body ++ ["endfunction"])) True
  where
    sig =
      [exon|function! #{name}(#{Text.intercalate ", " params})|]
