module Ribosome.Menu.Interpreter.MenuUi where

import Conc (Consume)
import Polysemy.Conc.Interpreter.Scoped (interpretScopedR_)

import qualified Ribosome.Menu.Effect.MenuUi as MenuUi
import Ribosome.Menu.Effect.MenuUi (MenuUi (PromptEvent, Render, RenderPrompt), NvimMenuUi)
import qualified Ribosome.Menu.Prompt.Data.PromptEvent as PromptEvent
import Ribosome.Menu.Prompt.Data.PromptEvent (PromptEvent)

interpretMenuUiNull :: InterpreterFor MenuUi r
interpretMenuUiNull =
  interpret \case
    RenderPrompt _ _ ->
      unit
    PromptEvent _ ->
      pure PromptEvent.Ignore
    Render _ ->
      unit

interpretMenuUiNvimNull :: InterpreterFor (NvimMenuUi ()) r
interpretMenuUiNvimNull =
  interpretScopedR_ (const unit) \ () -> \case
    RenderPrompt _ _ ->
      unit
    PromptEvent _ ->
      pure PromptEvent.Ignore
    Render _ ->
      unit

interceptMenuUiPromptConsume ::
  Members [MenuUi, Consume PromptEvent] r =>
  Sem r a ->
  Sem r a
interceptMenuUiPromptConsume =
  intercept \case
    RenderPrompt c p ->
      MenuUi.renderPrompt c p
    PromptEvent _ ->
      consume
    Render m ->
      MenuUi.render m

interceptMenuUiPromptEvents ::
  Members [MenuUi, EventConsumer pres PromptEvent] r =>
  Sem r a ->
  Sem r a
interceptMenuUiPromptEvents sem =
  subscribe do
    interceptMenuUiPromptConsume (raise sem)
