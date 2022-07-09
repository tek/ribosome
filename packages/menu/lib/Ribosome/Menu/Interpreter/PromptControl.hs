module Ribosome.Menu.Interpreter.PromptControl where

import Conc (interpretQueueTBM, interpretScopedWith_, interpretSync)
import qualified Queue
import qualified Sync

import Ribosome.Menu.Effect.PromptControl (PromptControl (..))
import Ribosome.Menu.Prompt.Data.PromptControlEvent (PromptControlEvent)
import Ribosome.Menu.Prompt.Data.PromptListening (PromptListening (PromptListening))
import Ribosome.Menu.Prompt.Data.PromptQuit (PromptQuit (PromptQuit))

type ScopeEffects =
  [Sync PromptQuit, Sync PromptListening, Queue PromptControlEvent]

scope ::
  Members [Resource, Race, Embed IO] r =>
  InterpretersFor ScopeEffects r
scope =
  interpretQueueTBM @PromptControlEvent 64 .
  interpretSync @PromptListening .
  interpretSync @PromptQuit

interpretPromptControl ::
  ∀ r .
  Members [Resource, Race, Embed IO] r =>
  InterpreterFor (Scoped () PromptControl) r
interpretPromptControl =
  interpretScopedWith_ @ScopeEffects scope \case
    QuitPrompt ->
      void (Sync.putTry PromptQuit)
    WaitPromptQuit ->
      void (Sync.block @PromptQuit)
    StartPrompt ->
      void (Sync.putTry PromptListening)
    WaitPromptListening ->
      void (Sync.block @PromptListening)
    ControlEvent ->
      Queue.readMaybe
    SendControlEvent e ->
      Queue.write e
