-- |The effect 'UserError' decides which messages to display in Neovim.
module Ribosome.Host.Effect.UserError where

import Polysemy.Log (Severity)

-- |The effect 'UserError' decides which messages to display in Neovim.
--
-- Additionally, the text may be manipulated, which is done by the interpreter in "Ribosome", which prefixes the message
-- with the plugin name.
data UserError :: Effect where
  -- |Decide whether and how to display the given message at the given log level.
  UserError :: Text -> Severity -> UserError m (Maybe [Text])

makeSem_ ''UserError

-- |Decide whether and how to display the given message at the given log level.
userError ::
  Member UserError r =>
  Text ->
  Severity ->
  Sem r (Maybe [Text])
