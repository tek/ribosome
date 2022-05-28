module Ribosome.Menu.Prompt where

import Polysemy.Time (MilliSeconds (MilliSeconds))

import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig (PromptConfig), PromptFlag)
import Ribosome.Menu.Prompt.Nvim (getCharStream, nvimPromptRenderer)
import Ribosome.Menu.Prompt.Transition (basicTransition)

defaultPrompt ::
  Members [Rpc, Rpc !! RpcError, Time t d, Race, Embed IO, Final IO] r =>
  [PromptFlag] ->
  Sem r (PromptConfig r)
defaultPrompt flags = do
  promptInput <- getCharStream (MilliSeconds 33)
  pure (PromptConfig promptInput basicTransition nvimPromptRenderer flags)
