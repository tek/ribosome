module Ribosome.Menu.Prompt where

import Ribosome.Control.Monad.Ribo (MonadRibo, NvimE)
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig (PromptConfig), PromptFlag)
import Ribosome.Menu.Prompt.Nvim (getCharStream, nvimPromptRenderer)
import Ribosome.Menu.Prompt.Transition (basicTransition)
import Ribosome.Msgpack.Error (DecodeError)

defaultPrompt ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepError e DecodeError m =>
  [PromptFlag] ->
  PromptConfig m
defaultPrompt =
  PromptConfig (getCharStream 0.033) basicTransition nvimPromptRenderer
