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
import Ribosome.Menu.Prompt.Data.PromptInputEvent (PromptInputEvent)
import Ribosome.Menu.Prompt.Input (promptInput, promptInputWith)
import Ribosome.Menu.Prompt.Nvim
import Ribosome.Menu.Stream.Util (queueStream)
import Ribosome.Menu.Data.PromptQuit (PromptQuit)

defaultPrompt ::
  Members [Sync PromptQuit, Rpc, Rpc !! RpcError, Time t d, Race, Embed IO, Final IO] r =>
  Sem r PromptInput
defaultPrompt =
  getCharStream (MilliSeconds 33)

defaultPromptConfig ::
  Members [Sync PromptQuit, Rpc, Rpc !! RpcError, Time t d, Race, Embed IO, Final IO] r =>
  [PromptFlag] ->
  Sem r PromptConfig
defaultPromptConfig flags = do
  p <- defaultPrompt
  pure (PromptConfig (Just p) flags)

queuePrompt ::
  Members [Queue PromptInputEvent, Final IO] r =>
  Sem r PromptInput
queuePrompt =
  PromptInput <$> queueStream
