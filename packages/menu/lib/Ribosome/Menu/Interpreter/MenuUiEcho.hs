module Ribosome.Menu.Interpreter.MenuUiEcho where

import Conc (
  interpretAtomic,
  interpretPScopedResumableWith,
  interpretPScopedResumableWithH,
  interpretQueueTBM,
  withAsync_,
  )
import qualified Data.Text as Text
import Exon (exon)
import Lens.Micro.Mtl (view)
import qualified Log
import qualified Queue

import Ribosome.Api.Window (closeWindow)
import Ribosome.Data.ScratchOptions (ScratchOptions)
import Ribosome.Data.ScratchState (ScratchState)
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Data.WindowConfig (WindowConfig (WindowConfig))
import qualified Ribosome.Effect.Scratch as Scratch
import Ribosome.Effect.Scratch (Scratch)
import qualified Ribosome.Effect.Settings as Settings
import Ribosome.Effect.Settings (Settings)
import Ribosome.Host.Api.Data (Window)
import Ribosome.Host.Api.Effect (nvimCallFunction, nvimWinGetConfig, vimGetWindows, windowSetOption)
import Ribosome.Host.Class.Msgpack.Encode (toMsgpack)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Menu.Data.NvimMenuState (NvimMenuState)
import Ribosome.Menu.Effect.MenuUi (
  EchoMenu (EchoMenu),
  MenuUi (PromptEvent, Render, RenderPrompt),
  NvimMenuConfig (NvimMenuConfig),
  NvimMenuUi,
  )
import Ribosome.Menu.Effect.PromptEvents (PromptEvents)
import Ribosome.Menu.Interpreter.PromptEvents (interpretPromptEventsDefault)
import Ribosome.Menu.NvimRenderer (menuSyntax, renderNvimMenu)
import Ribosome.Menu.Prompt.Data.Codes (decodeInputChar, decodeInputNum)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt (Prompt))
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig (OnlyInsert))
import qualified Ribosome.Menu.Prompt.Data.PromptEvent as PromptEvent
import Ribosome.Menu.Prompt.Data.PromptEvent (PromptEvent)
import qualified Ribosome.Menu.Prompt.Data.PromptMode as PromptMode
import Ribosome.Menu.Prompt.GetChar (processChar)
import Ribosome.Menu.Prompt.Nvim (nvimAcquire, nvimRelease, nvimRenderPrompt)
import Ribosome.Menu.Prompt.Run (pristinePrompt)
import qualified Ribosome.Menu.Settings as Settings

isFloat ::
  Member (Rpc !! RpcError) r =>
  Window ->
  Sem r Bool
isFloat win =
  False <! (check <$> nvimWinGetConfig win)
  where
    check (WindowConfig relative _ _) =
      not (Text.null relative)

closeFloats ::
  Members [Rpc, Rpc !! RpcError] r =>
  Sem r ()
closeFloats = do
  traverse_ closeWindow =<< filterM isFloat =<< vimGetWindows

withItemsScratch ::
  Members [Settings !! SettingError, Rpc, Rpc !! RpcError, Scratch, Stop RpcError, Log, Resource, Embed IO] r =>
  ScratchOptions ->
  (ScratchState -> Sem (AtomicState NvimMenuState : r) a) ->
  Sem r a
withItemsScratch options use = do
  whenM (Settings.or True Settings.menuCloseFloats) closeFloats
  bracket acquire (Scratch.kill . view #id) (interpretAtomic def . use)
  where
    acquire = do
      scratch <- Scratch.open (withSyntax options)
      windowSetOption (scratch ^. #window) "cursorline" True !>> Log.debug "Failed to set cursorline"
      pure scratch
    withSyntax =
      #syntax <>~ [menuSyntax]

data GetCharEvent =
  Set Prompt
  |
  Char Text
  deriving stock (Eq, Show)

type EchoScope =
  [PromptEvents, Queue GetCharEvent, AtomicState NvimMenuState]

quitChar :: Char
quitChar =
  '†'

quitCharOrd :: Int
quitCharOrd =
  ord quitChar

getChar ::
  Members [Queue GetCharEvent, Rpc !! RpcError, Log, Embed IO] r =>
  Sem r Bool
getChar =
  (event =<< getchar []) !>> do
    Log.debug "Prompt has quit"
    resume_ (void (getchar [toMsgpack True]))
    pure False
  where
    getchar =
      nvimCallFunction "getchar"
    event = \case
      Right c -> do
        Log.debug [exon|Nvim prompt char: #{c}|]
        True <$ Queue.write (Char (fromMaybe c (decodeInputChar c)))
      Left 0 -> do
        True <$ Log.debug "Nvim prompt: zero"
      Left num | num == quitCharOrd -> do
        Log.debug "Nvim prompt: quit char"
        pure False
      Left num -> do
        Log.debug [exon|Nvim prompt numeric: #{show num}|]
        traverse_ (Queue.write . Char) =<< decodeInputNum num
        pure True

withGetChar ::
  Members [Rpc !! RpcError, Log, Resource, Race, Async, Embed IO] r =>
  PromptConfig ->
  InterpretersFor [PromptEvents, Queue GetCharEvent] r
withGetChar conf ma =
  interpretQueueTBM 64 $ withAsync_ (Queue.write (Set (pristinePrompt conf)) *> whileM getChar *> Queue.close) do
    interpretPromptEventsDefault ma

echoResources ::
  Member (Stop RpcError) r =>
  Members [Settings !! SettingError, Rpc !! RpcError, Scratch !! RpcError, Log, Resource, Race, Async, Embed IO] r =>
  NvimMenuConfig ->
  (EchoMenu -> Sem (EchoScope ++ r) a) ->
  Sem r a
echoResources (NvimMenuConfig conf options _) use =
  restop @_ @Rpc $ restop @_ @Scratch $ withItemsScratch options \ scratchId ->
    bracket nvimAcquire nvimRelease \ _ ->
      withGetChar conf (insertAt @3 (use (EchoMenu conf scratchId)))

checkMode :: PromptConfig -> PromptEvent -> PromptEvent
checkMode OnlyInsert (PromptEvent.Update (Prompt _ PromptMode.Normal _)) =
  PromptEvent.Quit Nothing
checkMode _ e =
  e

promptEvent ::
  Members [Queue GetCharEvent, PromptEvents, Log] r =>
  PromptConfig ->
  Prompt ->
  Sem r PromptEvent
promptEvent conf prompt =
  Queue.readMaybe >>= \case
    Just (Set p) ->
      pure (PromptEvent.Update p)
    Just (Char c) ->
      checkMode conf <$> processChar c prompt
    Nothing ->
      pure (PromptEvent.Quit Nothing)

interpretMenuUiNvimEcho ::
  Members [Settings !! SettingError, Rpc !! RpcError, Scratch !! RpcError, Log, Resource, Race, Async, Embed IO] r =>
  InterpreterFor (NvimMenuUi EchoMenu) r
interpretMenuUiNvimEcho =
  interpretPScopedResumableWith @EchoScope echoResources \ (EchoMenu conf scr) -> \case
    RenderPrompt _ prompt ->
      restop (nvimRenderPrompt prompt)
    PromptEvent prompt ->
      promptEvent conf prompt
    Render menu ->
      runReader menu (renderNvimMenu scr)

onlyPromptEvents ::
  Members [Rpc !! RpcError, Log, Resource, Race, Async, Embed IO] r =>
  NvimMenuConfig ->
  (PromptConfig -> Sem (PromptEvents : Queue GetCharEvent : r) a) ->
  Sem r a
onlyPromptEvents (NvimMenuConfig conf _ _) use =
  withGetChar conf (use conf)

interpretGetCharMenuNvimNoRender ::
  Members [Rpc !! RpcError, Log, Resource, Race, Async, Embed IO] r =>
  InterpreterFor (NvimMenuUi PromptConfig) r
interpretGetCharMenuNvimNoRender =
  interpretPScopedResumableWithH @[PromptEvents, Queue GetCharEvent] onlyPromptEvents \ conf -> \case
    RenderPrompt _ _ ->
      unitT
    PromptEvent prompt ->
      pureT =<< promptEvent conf prompt
    Render _ ->
      unitT
