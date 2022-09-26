-- |API functions for applying syntax rules to Neovim.
module Ribosome.Api.Syntax where

import Ribosome.Data.Syntax.Syntax (Syntax)
import Ribosome.Host.Api.Data (Window, vimCallFunction)
import Ribosome.Host.Class.MonadRpc (MonadRpc (atomic))
import Ribosome.Host.Class.Msgpack.Encode (MsgpackEncode (toMsgpack))
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Modify (windo)
import Ribosome.Internal.Syntax (catCmds, syntaxCmds)

-- |Apply syntax rules to the current window.
executeSyntax ::
  MonadRpc m =>
  Syntax ->
  m ()
executeSyntax =
  catCmds . syntaxCmds

-- |Apply syntax rules to a window.
executeWindowSyntax ::
  Member Rpc r =>
  Window ->
  Syntax ->
  Sem r ()
executeWindowSyntax win syntax =
  windo win (executeSyntax syntax)

-- |Apply syntax rules to a window.
executeWindowSyntaxes ::
  MonadRpc m =>
  Window ->
  [Syntax] ->
  m ()
executeWindowSyntaxes win syntax =
  atomic $ windo win do
    traverse_ executeSyntax syntax

-- |Get the name of the syntax group at a given position.
syntaxName ::
  Member Rpc r =>
  Int ->
  Int ->
  Sem r (Text, Text)
syntaxName l c = do
  synId <- vimCallFunction "synID" (toMsgpack <$> [l, c, 0])
  (,) <$> vimCallFunction "getline" [toMsgpack l] <*> vimCallFunction "synIDattr" [synId, toMsgpack ("name" :: Text)]
