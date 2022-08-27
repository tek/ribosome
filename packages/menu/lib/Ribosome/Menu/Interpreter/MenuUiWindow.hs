module Ribosome.Menu.Interpreter.MenuUiWindow where

import Conc (Consume, Gate, GatesIO, interpretEventsChan, interpretPScopedResumableWith, withAsyncGated_)
import Control.Lens.Regex.Text (group, match, regex)
import Exon (exon)
import qualified Log
import Polysemy.Conc.Gate (signal)
import Prelude hiding (group)

import qualified Ribosome.Api.Mode as Api
import Ribosome.Api.Window (currentCursor, windowExec, setCursor)
import qualified Ribosome.Data.FloatOptions as FloatOptions
import Ribosome.Data.FloatOptions (FloatAnchor (SW), FloatRelative (Editor))
import Ribosome.Data.Mapping (Mapping, MappingId (MappingId), MappingLhs (MappingLhs), MappingSpec (MappingSpec))
import Ribosome.Data.Mode (NvimMode (NvimMode))
import Ribosome.Data.ScratchOptions (ScratchOptions, scratch)
import qualified Ribosome.Data.ScratchState as Scratch
import Ribosome.Data.SettingError (SettingError)
import qualified Ribosome.Effect.Scratch as Scratch
import Ribosome.Effect.Scratch (Scratch)
import Ribosome.Effect.Settings (Settings)
import Ribosome.Host.Api.Effect (
  nvimBufAttach,
  nvimCommand,
  nvimFeedkeys,
  nvimGetOption,
  nvimReplaceTermcodes,
  windowSetOption,
  )
import qualified Ribosome.Host.Api.Event as Event
import Ribosome.Host.Api.Event (pattern BufLinesEvent)
import Ribosome.Host.Class.Msgpack.Decode (pattern Msgpack)
import Ribosome.Host.Class.Msgpack.Map (msgpackMap)
import Ribosome.Host.Data.Event (Event (Event), EventName)
import Ribosome.Host.Data.RpcError (RpcError, rpcError)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Mapping (eventMapping)
import Ribosome.Menu.Data.NvimMenuState (NvimMenuState)
import Ribosome.Menu.Effect.MenuUi (
  MenuUi (PromptEvent, Render, RenderPrompt),
  NvimMenuConfig (NvimMenuConfig),
  WindowMenu (WindowMenu),
  WindowMenuUi,
  )
import Ribosome.Menu.Interpreter.MenuUiEcho (withItemsScratch)
import qualified Ribosome.Menu.Mappings as Mappings
import Ribosome.Menu.NvimRenderer (renderNvimMenu)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt (Prompt), PromptText (PromptText))
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig (OnlyInsert), isStartInsert)
import qualified Ribosome.Menu.Prompt.Data.PromptEvent as PromptEvent
import Ribosome.Menu.Prompt.Data.PromptEvent (PromptEvent)
import qualified Ribosome.Menu.Prompt.Data.PromptMode as PromptMode
import Ribosome.Menu.Prompt.Data.PromptMode (PromptMode)
import Ribosome.Data.ScratchState (ScratchState)

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
  Member Rpc r =>
  Sem r (Int, Int, Int, Int)
geometry = do
  width <- nvimGetOption "columns"
  height <- nvimGetOption "lines"
  let
    marginV =
      round (fromIntegral height * marginRatio)
    marginH =
      round (fromIntegral width * marginRatio)
  pure (height - marginV, marginH, height - 2 * marginV, width - 2 * marginH)
  where
    marginRatio =
      0.05 :: Float

itemsOptions :: (Int, Int, Int, Int) -> ScratchOptions -> ScratchOptions
itemsOptions (row, col, height, width) =
  (#float .~ Just floatOptions)
  .
  (#maxSize .~ Just (height - 3))
  where
    floatOptions =
      def
      & #relative .~ Editor
      & #anchor .~ SW
      & #row .~ row - 3
      & #col .~ col
      & #width .~ width
      & #height .~ 1
      & #border .~ FloatOptions.Manual ["╭", "─", "╮", "│", "", "", "", "│"]

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
      & #row .~ row
      & #col .~ col
      & #width .~ width
      & #height .~ 1
      & #border .~ FloatOptions.Manual ["│", "─", "│", "│", "╯", "─", "╰", "│"]

type WindowScope =
  [Consume PromptEvent, AtomicState NvimMenuState]

-- TODO use @:mapclear@ before setting mappings, as a flag in ScratchOptions. @nowait@ works fine to force menu
-- mappings, so no need to clear?
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
    subscribe @PromptEvent do
      void (nvimBufAttach (s ^. #buffer) False mempty)
      withAsyncGated_ (promptBufferEvents pconf) do
        insertAt @1 (use s)
  where
    acquire =
      Scratch.show @_ @[] [] (promptOptions geo mappings)
    release s = do
      Scratch.kill (Scratch.id s)
      key <- nvimReplaceTermcodes "<esc>" True False True
      nvimFeedkeys key "int" False

windowResources ::
  Member Log r =>
  Members [Settings !! SettingError, GatesIO, Embed IO] r =>
  Members [Scratch !! RpcError, Rpc !! RpcError, Stop RpcError, EventConsumer eres Event, Resource, Race, Async] r =>
  NvimMenuConfig ->
  (WindowMenu -> Sem (WindowScope ++ r) a) ->
  Sem r a
windowResources (NvimMenuConfig pconf options mappings) use =
  restop @_ @Rpc $ restop @_ @Scratch do
    geo <- geometry
    withItemsScratch (itemsOptions geo options) \ itemsScratch ->
      withPrompt pconf mappings geo \ promptScratch ->
        insertAt @2 (use (WindowMenu itemsScratch promptScratch))

-- TODO see how a 'buftype=prompt' looks
interpretMenuUiWindow ::
  Members [Log, GatesIO, Resource, Race, Async, Embed IO] r =>
  Members [Scratch !! RpcError, Rpc !! RpcError, Settings !! SettingError, EventConsumer eres Event] r =>
  InterpreterFor WindowMenuUi r
interpretMenuUiWindow =
  interpretPScopedResumableWith @WindowScope windowResources \ (WindowMenu itemsScratch promptScratch) -> \case
    RenderPrompt True (Prompt cursor _ (PromptText text)) -> do
      void (restop (Scratch.update (promptScratch ^. #id) ([text] :: [Text])))
      restop (setCursor (promptScratch ^. #window) 0 cursor)
    RenderPrompt False _ ->
      unit
    PromptEvent _ ->
      consume
    Render menu ->
      runReader menu (renderNvimMenu itemsScratch)
