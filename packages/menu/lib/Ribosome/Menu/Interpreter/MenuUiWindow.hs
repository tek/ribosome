module Ribosome.Menu.Interpreter.MenuUiWindow where

import Conc (
  Consume,
  Gate,
  GatesIO,
  interpretAtomic,
  interpretEventsChan,
  interpretPScopedResumableWith,
  withAsyncGated_,
  )
import Control.Lens.Regex.Text (group, match, regex)
import qualified Data.Text as Text
import Exon (exon)
import qualified Log
import Polysemy.Conc.Gate (signal)
import Prelude hiding (group)

import Ribosome.Api.Data (Window)
import qualified Ribosome.Api.Mode as Api
import Ribosome.Api.Window (closeWindow, currentCursor, setCursor, windowExec)
import qualified Ribosome.Data.FloatOptions as FloatOptions
import Ribosome.Data.FloatOptions (FloatAnchor (SW), FloatRelative (Editor))
import Ribosome.Data.Mapping (Mapping, MappingId (MappingId), MappingLhs (MappingLhs), MappingSpec (MappingSpec))
import Ribosome.Data.Mode (NvimMode (NvimMode))
import Ribosome.Data.ScratchOptions (ScratchOptions, scratch)
import qualified Ribosome.Data.ScratchState as Scratch
import Ribosome.Data.ScratchState (ScratchState)
import Ribosome.Data.SettingError (SettingError)
import qualified Ribosome.Data.WindowConfig as Nvim
import qualified Ribosome.Effect.Scratch as Scratch
import Ribosome.Effect.Scratch (Scratch)
import Ribosome.Effect.Settings (Settings)
import Ribosome.Host.Api.Data (Buffer)
import Ribosome.Host.Api.Effect (
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
import Ribosome.Menu.Data.NvimMenuState (NvimMenuState)
import Ribosome.Menu.Data.WindowConfig (WindowConfig (WindowConfig))
import Ribosome.Menu.Effect.MenuUi (
  MenuUi (PromptEvent, Render, RenderPrompt),
  WindowMenu (WindowMenu),
  WindowMenuUi,
  )
import qualified Ribosome.Menu.Mappings as Mappings
import Ribosome.Menu.NvimRenderer (menuSyntax, renderNvimMenu)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt (Prompt), PromptText (PromptText))
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig (OnlyInsert), isStartInsert)
import qualified Ribosome.Menu.Prompt.Data.PromptEvent as PromptEvent
import Ribosome.Menu.Prompt.Data.PromptEvent (PromptEvent)
import qualified Ribosome.Menu.Prompt.Data.PromptMode as PromptMode
import Ribosome.Menu.Prompt.Data.PromptMode (PromptMode)
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

interruptMapping :: MappingSpec
interruptMapping =
  Mappings.insert "<c-c>"

crMapping :: MappingSpec
crMapping =
  Mappings.insert "<cr>"

escMapping :: MappingSpec
escMapping =
  Mappings.insert "<esc>"

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

handleEsc ::
  Members [Rpc !! RpcError, EventConsumer eres Event, Events pres PromptEvent, Log] r =>
  PromptConfig ->
  Sem r ()
handleEsc pconf = do
  resuming (\ e -> Log.debug [exon|Menu window <esc>: #{rpcError e}|]) do
    Log.debug "MenuMapping: <esc>"
    if pconf == OnlyInsert
    then publish (PromptEvent.Quit Nothing)
    else do
      promptMode >>= traverse_ \case
        PromptMode.Normal ->
          publish (PromptEvent.Quit Nothing)
        PromptMode.Insert -> do
          nvimCommand "stopinsert"
          publish PromptEvent.Ignore

promptBufferEvents ::
  Members [Rpc !! RpcError, EventConsumer eres Event, Events pres PromptEvent, Log, Gate] r =>
  PromptConfig ->
  Sem r ()
promptBufferEvents pconf =
  subscribe @Event do
    Log.debug "Subscribed to buffer updates"
    signal
    forever do
      consume >>= \case
        BufLinesEvent {linedata = [text]} -> do
          resuming (\ e -> Log.debug [exon|Buffer event error: #{rpcError e}|]) do
            promptMode >>= traverse_ \ m -> do
              (_, cursor) <- currentCursor
              let prompt = Prompt cursor m (PromptText text)
              Log.debug [exon|BufLinesEvent: #{show prompt}|]
              publish (PromptEvent.Update prompt)
        MenuMapping (MappingId "<esc>") ->
          handleEsc pconf
        MenuMapping (MappingId lhs) -> do
          Log.debug [exon|MenuMapping: #{lhs}|]
          publish (PromptEvent.Mapping lhs)
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
itemsOptions (row, col, height, width) =
  (#float <|>~ Just floatOptions)
  .
  (#maxSize <|>~ Just (height - 5))
  where
    floatOptions =
      def
      & #relative .~ Editor
      & #anchor .~ SW
      & #row .~ row - 5
      & #col .~ col
      & #width .~ width
      & #height .~ 1
      & #border .~ FloatOptions.Manual ["╭", "─", "╮", "│", "", "", "", "│"]

statusOptions :: (Int, Int, Int, Int) -> ScratchOptions -> ScratchOptions
statusOptions (row, col, _, width) options =
  options
  & #float <|>~ Just floatOptions
  & #size <|>~ Just 1
  where
    floatOptions =
      def
      & #relative .~ Editor
      & #anchor .~ SW
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
  scratch "ribosome-prompt"
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
      & #row .~ row - 3
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
  ((ScratchState, Maybe ScratchState) -> Sem (AtomicState NvimMenuState : r) a) ->
  Sem r a
withMainScratch itemsOpt statusOpt use = do
  whenM (Settings.or True Settings.menuCloseFloats) closeFloats
  bracket acquire release (interpretAtomic def . use)
  where
    acquire = do
      itemScr <- Scratch.open (withItemsSyntax itemsOpt)
      windowSetOption (itemScr ^. #window) "cursorline" True !>> Log.debug "Failed to set cursorline"
      statusScr <- for statusOpt \ so -> do
        s <- Scratch.open (withStatusSyntax so)
        windowSetOption (s ^. #window) "cursorline" False
        pure s
      pure (itemScr, statusScr)
    release (it, stMay) = do
      Scratch.delete (it ^. #id)
      for_ stMay \ s -> Scratch.delete (s ^. #id)
    withItemsSyntax =
      #syntax <>~ [menuSyntax]
    withStatusSyntax =
      #syntax <>~ []

type WindowScope =
  [Consume PromptEvent, AtomicState NvimMenuState]

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
  Members [EventConsumer pres PromptEvent, Events pres PromptEvent] r =>
  Members [Rpc, Rpc !! RpcError, EventConsumer eres Event, Log, GatesIO, Resource, Race, Async] r =>
  PromptConfig ->
  Buffer ->
  Sem (Consume PromptEvent : r) a ->
  Sem r a
withBufferPromptEvents pconf buf sem =
  subscribe @PromptEvent do
    void (nvimBufAttach buf False mempty)
    withAsyncGated_ (promptBufferEvents pconf) do
      sem

withPrompt ::
  Member (EventConsumer eres Event) r =>
  Members [Scratch, Rpc, Rpc !! RpcError, Stop RpcError, Log, Resource, GatesIO, Race, Async, Embed IO] r =>
  PromptConfig ->
  [MappingSpec] ->
  (Int, Int, Int, Int) ->
  (ScratchState -> Sem (Consume PromptEvent : r) a) ->
  Sem r a
withPrompt pconf mappings geo use =
  interpretEventsChan @PromptEvent $ bracket acquire release \ s -> do
    when (isStartInsert pconf) (windowExec (s ^. #window) "startinsert")
    windowSetOption (s ^. #window) "cursorline" False
    withBufferPromptEvents pconf (s ^. #buffer) do
      insertAt @1 (use s)
  where
    acquire =
      Scratch.show @_ @[] [] (promptOptions geo mappings)
    release s = do
      Scratch.delete (Scratch.id s)
      flushInput

windowResources ::
  Member Log r =>
  Members [Settings !! SettingError, GatesIO, Embed IO] r =>
  Members [Scratch !! RpcError, Rpc !! RpcError, Stop RpcError, EventConsumer eres Event, Resource, Race, Async] r =>
  WindowConfig ->
  (WindowMenu -> Sem (WindowScope ++ r) a) ->
  Sem r a
windowResources (WindowConfig pconf itemsOpt statusOpt mappings) use =
  restop @_ @Rpc $ restop @_ @Scratch do
    geo <- geometry
    withMainScratch (itemsOptions geo itemsOpt) (statusOptions geo <$> statusOpt) \ (itemsScratch, statusScratch) ->
      withPrompt pconf mappings geo \ promptScratch ->
        insertAt @2 (use (WindowMenu itemsScratch statusScratch promptScratch))

-- TODO see how a 'buftype=prompt' looks
interpretMenuUiWindow ::
  Members [Log, GatesIO, Resource, Race, Async, Embed IO] r =>
  Members [Scratch !! RpcError, Rpc !! RpcError, Settings !! SettingError, EventConsumer eres Event] r =>
  InterpreterFor WindowMenuUi r
interpretMenuUiWindow =
  interpretPScopedResumableWith @WindowScope windowResources \ (WindowMenu itemsScratch statusScratch promptScratch) -> \case
    RenderPrompt True (Prompt cursor _ (PromptText text)) -> do
      void (restop (Scratch.update (promptScratch ^. #id) ([text] :: [Text])))
      restop (setCursor (promptScratch ^. #window) 0 cursor)
    RenderPrompt False _ ->
      unit
    PromptEvent _ ->
      consume
    Render menu ->
      runReader menu (renderNvimMenu itemsScratch statusScratch)
