module Ribosome.Menu.Prompt where

import Ribosome.Control.Monad.Ribo (MonadRibo, NvimE)
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig(PromptConfig))
import Ribosome.Menu.Prompt.Nvim (getCharC, nvimPromptRenderer)
import Ribosome.Menu.Prompt.Run (basicTransition)
import Ribosome.Msgpack.Error (DecodeError)

defaultPrompt ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepError e DecodeError m =>
  PromptConfig m
defaultPrompt =
  PromptConfig (getCharC 0.033) basicTransition nvimPromptRenderer False
