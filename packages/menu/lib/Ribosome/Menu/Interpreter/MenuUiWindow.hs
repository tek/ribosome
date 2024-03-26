module Ribosome.Menu.Interpreter.MenuUiWindow where

import Conc (
  Consume,
  Gate,
  Gates,
  Lock,
  interpretAtomic,
  interpretEventsChan,
  interpretLockReentrant,
  lock,
  withAsyncGated_,
  )
import Control.Lens.Regex.Text (group, match, regex)
import qualified Data.Text as Text
import Exon (exon)
import qualified Log
import Polysemy.Conc.Gate (signal)
import Prelude hiding (group)

import qualified Ribosome.Api.Mode as Api
import Ribosome.Api.Option (withOption)
import Ribosome.Api.Window (closeWindow, currentCursor, setCursor)
import qualified Ribosome.Data.FloatOptions as FloatOptions
import Ribosome.Data.FloatOptions (FloatAnchor (NW, SW), FloatRelative (Editor))
import Ribosome.Data.Mapping (
  MapMode (MapInsert),
  Mapping,
  MappingId (MappingId),
  MappingLhs (MappingLhs),
  MappingSpec (MappingSpec),
  )
import Ribosome.Data.Mode (NvimMode (NvimMode))
import Ribosome.Data.ScratchOptions (ScratchOptions, ensureName, scratch)
import qualified Ribosome.Data.ScratchState as Scratch
import Ribosome.Data.ScratchState (ScratchState)
import Ribosome.Data.SettingError (SettingError)
import qualified Ribosome.Data.WindowConfig as Nvim
import qualified Ribosome.Effect.Scratch as Scratch
import Ribosome.Effect.Scratch (Scratch)
import Ribosome.Effect.Settings (Settings)
import Ribosome.Host.Api.Data (
  Buffer,
  Window,
  bufferSetOption,
  nvimBufAttach,
  nvimCommand,
  nvimFeedkeys,
  nvimGetOption,
  nvimReplaceTermcodes,
  nvimWinGetConfig,
  vimGetWindows,
  windowSetOption,
  )
import qualified Ribosome.Host.Api.Event as Event
import Ribosome.Host.Api.Event (pattern BufLinesEvent)
import Ribosome.Host.Class.Msgpack.Decode (pattern Msgpack)
import Ribosome.Host.Class.Msgpack.Map (msgpackMap)
import Ribosome.Host.Data.Event (Event (Event), EventName)
import Ribosome.Host.Data.RpcError (RpcError, rpcError)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Lens ((<|>~))
import Ribosome.Mapping (eventMapping)
import Ribosome.Menu.Data.MenuView (MenuView)
import Ribosome.Menu.Data.WindowConfig (WindowConfig (WindowConfig))
import Ribosome.Menu.Effect.MenuUi (
  MenuUi (ItemsScratch, PromptEvent, PromptScratch, Render, RenderPrompt, StatusScratch),
  WindowMenu (WindowMenu),
  )
import Ribosome.Menu.NvimRenderer (menuSyntax, renderNvimMenu)
import Ribosome.Menu.Prompt.Data.Prompt (CursorCol (CursorCol), Prompt (Prompt), PromptText (PromptText))
import qualified Ribosome.Menu.Prompt.Data.PromptEvent as PromptEvent
import Ribosome.Menu.Prompt.Data.PromptEvent (PromptEvent)
import qualified Ribosome.Menu.Prompt.Data.PromptMode as PromptMode
import Ribosome.Menu.Prompt.Data.PromptMode (PromptMode)
import Ribosome.Menu.Scratch (menuItemsScratchId)
import qualified Ribosome.Menu.Settings as Settings
import qualified Ribosome.Settings as Settings

pattern MenuMapping :: MappingId -> Event
pattern MenuMapping {id} <- Event "menu-mapping" [Msgpack id]

escapeLt :: Text -> Text
escapeLt =
  [regex|<|] . match .~ "<lt>"

stripTermcode :: Text -> Text
stripTermcode t =
  fromMaybe t (t ^? [regex|<(.*)>|] . group 0)

insertSpec :: MappingLhs -> MappingSpec
insertSpec lhs =
  MappingSpec lhs [MapInsert]

interruptMapping :: MappingSpec
interruptMapping =
  insertSpec "<c-c>"

crMapping :: MappingSpec
crMapping =
  insertSpec "<cr>"

escMapping :: MappingSpec
escMapping =
  insertSpec "<esc>"

mappingEvent :: EventName
mappingEvent =
  "menu-mapping"

promptMode ::
  Member Rpc r =>
  Sem r (Maybe PromptMode)
promptMode =
  Api.mode <&> \case
    NvimMode "n" _ ->
      Just PromptMode.Normal
    NvimMode "i" _ ->
      Just PromptMode.Insert
    _ ->
      Nothing

lockAs ::
  ∀ k r a .
  Member (Lock @@ k) r =>
  Sem r a ->
  Sem r a
lockAs =
  tag @k . lock . raise

publishPrompt ::
  Members [Events PromptEvent, Rpc !! RpcError, Lock @@ "prompt", Log] r =>
  Text ->
  Sem r ()
publishPrompt text =
  resuming logError do
    lockAs @"prompt" $ promptMode >>= traverse_ \ m -> do
      (_, cursor) <- currentCursor
      let prompt = Prompt (CursorCol cursor) m (PromptText text)
      Log.debug [exon|BufLinesEvent: #{show prompt}|]
      publish (PromptEvent.Update prompt)
  where
    logError e = Log.debug [exon|Buffer event error: #{rpcError e}|]

setMode ::
  Member Rpc r =>
  PromptMode ->
  Sem r ()
setMode = \case
  PromptMode.Normal -> nvimCommand "stopinsert"
  PromptMode.Insert -> nvimCommand "startinsert"

updateMode ::
  Members [Rpc, Log] r =>
  PromptMode ->
  Sem r ()
updateMode target = do
  current <- promptMode
  unless (current == Just target) do
    Log.debug [exon|Updating window mode to #{show target}|]
    setMode target

updatePrompt ::
  Members [Scratch !! RpcError, Rpc !! RpcError, Stop RpcError, Log] r =>
  ScratchState ->
  Prompt ->
  Sem r ()
updatePrompt promptScratch (Prompt (CursorCol cursor) mode (PromptText text)) = do
  void (restop (Scratch.update promptScratch.id ([text] :: [Text])))
  restop @RpcError @Rpc do
    updateMode mode
    setCursor promptScratch.window 0 cursor

promptBufferEvents ::
  Members [Rpc !! RpcError, EventConsumer Event, Events PromptEvent, Lock @@ "prompt", Log, Gate] r =>
  Sem r ()
promptBufferEvents =
  subscribe @Event do
    Log.debug "Subscribed to buffer updates"
    signal
    forever do
      consume >>= \case
        BufLinesEvent {linedata = [text]} ->
          publishPrompt text
        MenuMapping (MappingId lhs) -> do
          Log.debug [exon|MenuMapping: #{lhs}|]
          publish (PromptEvent.Mapping (MappingLhs lhs))
        _ ->
          unit

promptMappings ::
  [MappingSpec] ->
  [Mapping]
promptMappings custom =
  mapping <$> specs
  where
    specs =
      crMapping : escMapping : interruptMapping : custom
    mapping (MappingSpec lhs@(MappingLhs mid) mode) =
      eventMapping mappingEvent lhs mode (Just (MappingId (escapeLt mid))) (msgpackMap ("nowait", True))

geometry ::
  Members [Settings !! SettingError, Rpc] r =>
  Sem r (Int, Int, Int, Int)
geometry = do
  marginRatioV <- Settings.or 0.2 Settings.menuMarginVertical
  marginRatioH <- Settings.or 0.1 Settings.menuMarginHorizontal
  width <- nvimGetOption "columns"
  height <- nvimGetOption "lines"
  let
    marginV =
      round (fromIntegral height * marginRatioV)
    marginH =
      round (fromIntegral width * marginRatioH)
  pure (height - marginV, marginH, height - 2 * marginV, width - 2 * marginH)

itemsOptions :: (Int, Int, Int, Int) -> ScratchOptions -> ScratchOptions
itemsOptions (row, col, height, width) options =
  ensureName menuItemsScratchId options
  & #float <|>~ Just floatOptions
  & #maxSize <|>~ Just (height - 5)
  where
    floatOptions =
      def
      & #relative .~ Editor
      & #anchor .~ SW
      & #row .~ row - 2
      & #col .~ col
      & #width .~ width
      & #height .~ 1
      & #border .~ FloatOptions.Manual ["╭", "─", "╮", "│", "", "", "", "│"]

statusOptions :: (Int, Int, Int, Int) -> ScratchOptions -> ScratchOptions
statusOptions (row, col, _, width) options =
  ensureName "ribosome-menu-status" options
  & #float <|>~ Just floatOptions
  & #size <|>~ Just 1
  where
    floatOptions =
      def
      & #relative .~ Editor
      & #anchor .~ NW
      & #row .~ row
      & #col .~ col
      & #width .~ width
      & #height .~ 1
      & #border .~ FloatOptions.Manual ["│", "─", "│", "│", "╯", "─", "╰", "│"]

promptOptions ::
  (Int, Int, Int, Int) ->
  [MappingSpec] ->
  ScratchOptions
promptOptions (row, col, _, width) custom =
  scratch "ribosome-menu-prompt"
  & #modify .~ True
  & #focus .~ True
  & #mappings <>~ promptMappings custom
  & #size .~ Just 1
  & #maxSize .~ Just 1
  & #float .~ Just floatOptions
  where
    floatOptions =
      def
      & #relative .~ Editor
      & #anchor .~ SW
      & #row .~ row
      & #col .~ col
      & #width .~ width
      & #height .~ 1
      & #border .~ FloatOptions.Manual ["│", "─", "│", "│", "", "", "", "│"]

isFloat ::
  Member (Rpc !! RpcError) r =>
  Window ->
  Sem r Bool
isFloat win =
  False <! (check <$> nvimWinGetConfig win)
  where
    check (Nvim.WindowConfig relative _ _) =
      not (Text.null relative)

closeFloats ::
  Members [Rpc, Rpc !! RpcError] r =>
  Sem r ()
closeFloats = do
  traverse_ closeWindow =<< filterM isFloat =<< vimGetWindows

withMainScratch ::
  Members [Settings !! SettingError, Rpc, Rpc !! RpcError, Scratch, Stop RpcError, Log, Resource, Embed IO] r =>
  ScratchOptions ->
  Maybe ScratchOptions ->
  ((ScratchState, Maybe ScratchState) -> Sem (AtomicState MenuView : r) a) ->
  Sem r a
withMainScratch itemsOpt statusOpt use = do
  whenM (Settings.or True Settings.menuCloseFloats) closeFloats
  bracket acquire release (interpretAtomic def . use)
  where
    acquire = do
      itemScr <- Scratch.open (withItemsSyntax itemsOpt)
      windowSetOption itemScr.window "cursorline" True
      statusScr <- for statusOpt \ so -> do
        s <- Scratch.open (withStatusSyntax so)
        windowSetOption (s.window) "cursorline" False
        pure s
      pure (itemScr, statusScr)
    release (it, stMay) = do
      Scratch.delete (it.id)
      for_ stMay \ s -> Scratch.delete (s.id)
    withItemsSyntax =
      #syntax <>~ [menuSyntax]
    withStatusSyntax =
      #syntax <>~ []

-- For some reason, Neovim stays in expectation of a typed character after the menu was closed, so this sends another
-- @<esc>@.
--
-- Might be due to some legacy stuff left over from the getchar prompt, like @inputrestore@, needs investigation.
flushInput ::
  Member Rpc r =>
  Sem r ()
flushInput = do
  key <- nvimReplaceTermcodes "<esc>" True False True
  nvimFeedkeys key "int" False

withBufferPromptEvents ::
  Members [EventConsumer PromptEvent, Events PromptEvent, Lock @@ "prompt"] r =>
  Members [Rpc, Rpc !! RpcError, EventConsumer Event, Log, Gates, Resource, Race, Async] r =>
  Buffer ->
  Sem (Consume PromptEvent : r) a ->
  Sem r a
withBufferPromptEvents buf sem =
  subscribe @PromptEvent do
    void (nvimBufAttach buf False mempty)
    withAsyncGated_ promptBufferEvents do
      sem

withPrompt ::
  Members [EventConsumer Event, Lock @@ "prompt"] r =>
  Members [Scratch, Rpc, Rpc !! RpcError, Stop RpcError, Log, Resource, Gates, Race, Async, Embed IO] r =>
  [MappingSpec] ->
  (Int, Int, Int, Int) ->
  (ScratchState -> Sem (Consume PromptEvent : r) a) ->
  Sem r a
withPrompt mappings geo use =
  interpretEventsChan @PromptEvent $ bracket acquire release \ s -> do
    windowSetOption s.window "cursorline" False
    bufferSetOption s.buffer "textwidth" (0 :: Int64)
    withBufferPromptEvents s.buffer do
      insertAt @1 (use s)
  where
    acquire =
      Scratch.show @_ @[] [] (promptOptions geo mappings)
    release s = do
      Scratch.delete s.id
      flushInput

type WindowScope =
  [Consume PromptEvent, AtomicState MenuView, Lock @@ "prompt"]

windowResources ::
  Member Log r =>
  Members [Settings !! SettingError, Gates, Embed IO] r =>
  Members [Scratch !! RpcError, Rpc !! RpcError, Stop RpcError, EventConsumer Event, Resource, Race, Async, Mask] r =>
  WindowConfig ->
  (WindowMenu -> Sem (WindowScope ++ r) a) ->
  Sem r a
windowResources (WindowConfig itemsOpt statusOpt mappings) use =
  restop @_ @Rpc $ restop @_ @Scratch $ interpretLockReentrant $ untag do
    geo <- geometry
    withMainScratch (itemsOptions geo itemsOpt) (statusOptions geo <$> statusOpt) \ (itemsScratch, statusScratch) ->
      withPrompt mappings geo \ promptScratch ->
        withOption "hlsearch" False do
          insertAt @3 (use (WindowMenu itemsScratch statusScratch promptScratch))

-- TODO see how a 'buftype=prompt' looks
interpretMenuUiWindow ::
  Members [Log, Gates, Resource, Race, Async, Mask, Embed IO] r =>
  Members [Scratch !! RpcError, Rpc !! RpcError, Settings !! SettingError, EventConsumer Event] r =>
  InterpreterFor (Scoped WindowConfig (MenuUi !! RpcError) !! RpcError) r
interpretMenuUiWindow =
  interpretScopedRWith @WindowScope windowResources \ (WindowMenu itemsScratch statusScratch promptScratch) -> \case
    RenderPrompt True prompt -> do
      tag @"prompt" $ lock $ updatePrompt promptScratch prompt
    RenderPrompt False _ ->
      unit
    PromptEvent ->
      consume
    Render menu ->
      runReader menu (renderNvimMenu itemsScratch statusScratch)
    PromptScratch ->
      stopNote "Prompt scratch not found" =<< restop (Scratch.find "ribosome-menu-prompt")
    StatusScratch ->
      stopNote "Status scratch not found" =<< restop (Scratch.find "ribosome-menu-status")
    ItemsScratch ->
      stopNote "Items scratch not found" =<< restop (Scratch.find menuItemsScratchId)
