module Ribosome.Menu.Prompt (
  module Ribosome.Menu.Prompt,
  module Ribosome.Menu.Prompt.Data.PromptConfig,
  module Ribosome.Menu.Prompt.Input,
  module Ribosome.Menu.Prompt.Nvim,
) where

import Polysemy.Time (MilliSeconds (MilliSeconds))

import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Menu.Prompt.Data.PromptConfig
import Ribosome.Menu.Prompt.Input (promptInput, promptInputWith)
import Ribosome.Menu.Prompt.Nvim

defaultPrompt ::
  Members [Rpc, Rpc !! RpcError, Time t d, Race, Embed IO, Final IO] r =>
  [PromptFlag] ->
  Sem r PromptConfig
defaultPrompt fs = do
  inputStream <- getCharStream (MilliSeconds 33)
  pure (PromptConfig inputStream fs)
