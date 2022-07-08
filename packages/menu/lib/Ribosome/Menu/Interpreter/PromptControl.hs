module Ribosome.Menu.Interpreter.PromptControl where

import Conc (interpretQueueTBM, interpretSync)
import Polysemy.Internal.Tactics (liftT)
import qualified Queue
import qualified Sync

import Ribosome.Menu.Effect.PromptControl (PromptControl (..))
import Ribosome.Menu.Prompt.Data.PromptControlEvent (PromptControlEvent)
import Ribosome.Menu.Prompt.Data.PromptListening (PromptListening (PromptListening))
import Ribosome.Menu.Prompt.Data.PromptQuit (PromptQuit (PromptQuit))

liftT_ ::
  Functor f =>
  Sem r a ->
  Sem (WithTactics e f m r) (f ())
liftT_ =
  liftT . void

interpretPromptControl ::
  ∀ mres r .
  Members [Resource, Race, Mask mres, Embed IO] r =>
  InterpreterFor PromptControl r
interpretPromptControl =
  interpretQueueTBM @PromptControlEvent 64 .
  interpretSync @PromptListening .
  interpretSync @PromptQuit .
  reinterpret3 \case
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
