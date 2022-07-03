module Ribosome.Menu.Prompt (
  module Ribosome.Menu.Prompt.Nvim,
  module Ribosome.Menu.Effect.PromptInput,
  module Ribosome.Menu.Interpreter.PromptInput,
  module Ribosome.Menu.Prompt.Data.PromptListening,
  module Ribosome.Menu.Prompt.Data.PromptQuit,
) where

import Polysemy.Time (MilliSeconds (MilliSeconds))

import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Menu.Effect.PromptInput
import Ribosome.Menu.Interpreter.PromptInput
import Ribosome.Menu.Prompt.Data.PromptFlag
import Ribosome.Menu.Prompt.Data.PromptInputEvent (PromptInputEvent)
import Ribosome.Menu.Prompt.Data.PromptListening
import Ribosome.Menu.Prompt.Data.PromptQuit
import Ribosome.Menu.Prompt.Nvim
import Ribosome.Menu.Stream.Util (queueStream)

-- defaultPrompt ::
--   Members [Sync PromptQuit, Rpc, Rpc !! RpcError, Time t d, Race, Embed IO, Final IO] r =>
--   Sem r PromptInput
-- defaultPrompt =
--   getCharStream (MilliSeconds 33)

-- queuePrompt ::
--   Members [Queue PromptInputEvent, Final IO] r =>
--   Sem r PromptInput
-- queuePrompt =
--   PromptInput <$> queueStream
