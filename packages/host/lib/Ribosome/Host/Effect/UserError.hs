module Ribosome.Host.Effect.UserError where

import Polysemy.Log (Severity)

data UserError :: Effect where
  UserError :: Text -> Severity -> UserError m (Maybe [Text])

makeSem ''UserError
