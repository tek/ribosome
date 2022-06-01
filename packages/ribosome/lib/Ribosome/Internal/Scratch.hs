module Ribosome.Internal.Scratch where

import Control.Lens ((^.))
import Data.Generics.Labels ()
import qualified Data.Map.Strict as Map
import Data.MessagePack (Object)
import Exon (exon)
import qualified Polysemy.Log as Log

import Ribosome.Api.Autocmd (bufferAutocmd, eventignore)
import Ribosome.Api.Buffer (setBufferContent, wipeBuffer)
import Ribosome.Api.Syntax (executeWindowSyntax)
import Ribosome.Api.Tabpage (closeTabpage)
import Ribosome.Api.Window (closeWindow)
import Ribosome.Data.FloatOptions (FloatOptions, enter)
import Ribosome.Data.PluginName (PluginName (PluginName))
import Ribosome.Data.ScratchId (ScratchId (ScratchId, unScratchId))
import Ribosome.Data.ScratchOptions (ScratchOptions (ScratchOptions))
import qualified Ribosome.Data.ScratchState as ScratchState
import Ribosome.Data.ScratchState (ScratchState (ScratchState))
import Ribosome.Host.Api.Data (Buffer, Tabpage, Window)
import Ribosome.Host.Api.Effect (
  bufferGetName,
  bufferGetNumber,
  bufferSetName,
  bufferSetOption,
  nvimBufIsLoaded,
  nvimCreateBuf,
  nvimOpenWin,
  vimCommand,
  vimGetCurrentBuffer,
  vimGetCurrentTabpage,
  vimGetCurrentWindow,
  vimSetCurrentWindow,
  windowGetBuffer,
  windowIsValid,
  windowSetHeight,
  windowSetOption,
  windowSetWidth,
  )
import Ribosome.Host.Class.Msgpack.Decode (fromMsgpack)
import Ribosome.Host.Class.Msgpack.Encode (toMsgpack)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Mapping (activateBufferMapping)
import Ribosome.PluginName (pluginNameCapitalized)

createScratchTab :: Member Rpc r => Sem r Tabpage
createScratchTab = do
  vimCommand "tabnew"
  vimGetCurrentTabpage

createRegularWindow ::
  Member Rpc r =>
  Bool ->
  Bool ->
  Maybe Int ->
  Sem r (Buffer, Window)
createRegularWindow vertical bottom size = do
  vimCommand prefixedCmd
  buf <- vimGetCurrentBuffer
  win <- vimGetCurrentWindow
  pure (buf, win)
  where
    prefixedCmd = locationPrefix <> " " <> sizePrefix <> cmd
    cmd = if vertical then "vnew" else "new"
    sizePrefix = maybe "" show size
    locationPrefix = if bottom then "belowright" else "aboveleft"

floatConfig ::
  FloatOptions ->
  Map Text Object
floatConfig =
  fromRight Map.empty . fromMsgpack . toMsgpack

createFloatWith ::
  Member Rpc r =>
  Bool ->
  Bool ->
  FloatOptions ->
  Sem r (Buffer, Window)
createFloatWith listed scratch options = do
  buffer <- nvimCreateBuf listed scratch
  window <- nvimOpenWin buffer (enter options) (floatConfig options)
  pure (buffer, window)

createFloat ::
  Member Rpc r =>
  FloatOptions ->
  Sem r (Buffer, Window)
createFloat =
  createFloatWith True True

createScratchWindow ::
  Member Rpc r =>
  Bool ->
  Bool ->
  Bool ->
  Maybe FloatOptions ->
  Maybe Int ->
  Sem r (Buffer, Window)
createScratchWindow vertical wrap bottom float size = do
  (buffer, win) <- createWindow
  windowSetOption win "wrap" (toMsgpack wrap)
  windowSetOption win "number" (toMsgpack False)
  windowSetOption win "cursorline" (toMsgpack True)
  windowSetOption win "colorcolumn" (toMsgpack ("" :: Text))
  windowSetOption win "foldmethod" (toMsgpack ("manual" :: Text))
  windowSetOption win "conceallevel" (toMsgpack (2 :: Int))
  windowSetOption win "concealcursor" (toMsgpack ("nvic" :: Text))
  pure (buffer, win)
  where
    createWindow =
      maybe regular createFloat float
    regular =
      createRegularWindow vertical bottom size

createScratchUiInTab :: Member Rpc r => Sem r (Buffer, Window, Maybe Tabpage)
createScratchUiInTab = do
  tab <- createScratchTab
  win <- vimGetCurrentWindow
  buffer <- windowGetBuffer win
  pure (buffer, win, Just tab)

createScratchUi ::
  Member Rpc r =>
  ScratchOptions ->
  Sem r (Buffer, Window, Maybe Tabpage)
createScratchUi (ScratchOptions False vertical wrap _ _ bottom _ float size _ _ _ _ _) =
  uncurry (,,Nothing) <$> createScratchWindow vertical wrap bottom float size
createScratchUi _ =
  createScratchUiInTab

configureScratchBuffer ::
  Member Rpc r =>
  Buffer ->
  Maybe Text ->
  ScratchId ->
  Sem r ()
configureScratchBuffer buffer ft (ScratchId name) = do
  bufferSetOption buffer "bufhidden" ("wipe" :: Text)
  bufferSetOption buffer "buftype" ("nofile" :: Text)
  bufferSetOption buffer "swapfile" False
  traverse_ (bufferSetOption buffer "filetype") ft
  bufferSetName buffer name

setupScratchBuffer ::
  Members [Rpc, Log] r =>
  Window ->
  Buffer ->
  Maybe Text ->
  ScratchId ->
  Sem r Buffer
setupScratchBuffer window buffer ft name = do
  valid <- nvimBufIsLoaded buffer
  Log.debug [exon|#{if valid then "" else "in"}valid scratch buffer|]
  validBuffer <- if valid then pure buffer else windowGetBuffer window
  configureScratchBuffer validBuffer ft name
  pure validBuffer

setupDeleteAutocmd ::
  Members [Rpc, Reader PluginName] r =>
  ScratchState ->
  Sem r ()
setupDeleteAutocmd (ScratchState name _ buffer _ _ _) = do
  PluginName pname <- pluginNameCapitalized
  bufferAutocmd buffer "RibosomeScratch" "BufDelete" (deleteCall pname)
  where
    deleteCall pname =
      [exon|silent! call #{pname}DeleteScratch('#{coerce name}')|]

setupScratchIn ::
  Members [Rpc, AtomicState (Map ScratchId ScratchState), Reader PluginName, Log] r =>
  Buffer ->
  Window ->
  Window ->
  Maybe Tabpage ->
  ScratchOptions ->
  Sem r ScratchState
setupScratchIn buffer previous window tab options@(ScratchOptions _ _ _ focus _ _ _ _ _ _ syntax mappings ft name) = do
  validBuffer <- setupScratchBuffer window buffer ft name
  traverse_ (executeWindowSyntax window) syntax
  traverse_ (activateBufferMapping validBuffer) mappings
  unless focus $ vimSetCurrentWindow previous
  let scratch = ScratchState name options validBuffer window previous tab
  atomicModify' (Map.insert name scratch)
  setupDeleteAutocmd scratch
  pure scratch

createScratch ::
  Members [Rpc, AtomicState (Map ScratchId ScratchState), Reader PluginName, Log, Resource] r =>
  ScratchOptions ->
  Sem r ScratchState
createScratch options = do
  Log.debug [exon|creating new scratch: #{show options}|]
  previous <- vimGetCurrentWindow
  (buffer, window, tab) <- eventignore $ createScratchUi options
  eventignore $ setupScratchIn buffer previous window tab options

bufferStillLoaded ::
  Members [Rpc !! RpcError, Rpc] r =>
  ScratchId ->
  Buffer ->
  Sem r Bool
bufferStillLoaded (ScratchId name) buffer =
  (&&) <$> loaded <*> loadedName
  where
    loaded =
      nvimBufIsLoaded buffer
    loadedName =
      resumeAs @RpcError False ((name ==) <$> bufferGetName buffer)

updateScratch ::
  Members [Rpc !! RpcError, Rpc, AtomicState (Map ScratchId ScratchState), Reader PluginName, Log, Resource] r =>
  ScratchState ->
  ScratchOptions ->
  Sem r ScratchState
updateScratch oldScratch@(ScratchState name _ oldBuffer oldWindow _ _) options = do
  Log.debug [exon|updating existing scratch '#{coerce name}'|]
  ifM (windowIsValid oldWindow) attemptReuseWindow reset
  where
    attemptReuseWindow =
      ifM (bufferStillLoaded name oldBuffer) (pure oldScratch) closeAndReset
    closeAndReset =
      closeWindow oldWindow *> reset
    reset =
      createScratch options

lookupScratch ::
  Member (AtomicState (Map ScratchId ScratchState)) r =>
  ScratchId ->
  Sem r (Maybe ScratchState)
lookupScratch name =
  atomicGets (Map.lookup name)

ensureScratch ::
  Members [Rpc !! RpcError, Rpc, AtomicState (Map ScratchId ScratchState), Reader PluginName, Log, Resource] r =>
  ScratchOptions ->
  Sem r ScratchState
ensureScratch options = do
  f <- maybe createScratch updateScratch <$> lookupScratch (options ^. #name)
  f options

withModifiable ::
  Member Rpc r =>
  Buffer ->
  ScratchOptions ->
  Sem r a ->
  Sem r a
withModifiable buffer options thunk =
  if isWrite then thunk else wrap
  where
    isWrite =
      options ^. #modify
    wrap =
      update True *> thunk <* update False
    update =
      bufferSetOption buffer "modifiable"

setScratchContent ::
  Foldable t =>
  Members [Rpc !! RpcError, Rpc] r =>
  ScratchState ->
  t Text ->
  Sem r ()
setScratchContent (ScratchState _ options buffer win _ _) lines' = do
  withModifiable buffer options $ setBufferContent buffer (toList lines')
  when (options ^. #resize) (resume_ @RpcError @Rpc (setSize win size))
  where
    size =
      max 1 calculateSize
    calculateSize =
      if vertical then fromMaybe 50 maxSize else min (length lines') (fromMaybe 30 maxSize)
    maxSize =
      options ^. #maxSize
    vertical =
      options ^. #vertical
    setSize =
      if vertical then windowSetWidth else windowSetHeight

showInScratch ::
  Foldable t =>
  Members [Rpc !! RpcError, Rpc, AtomicState (Map ScratchId ScratchState), Reader PluginName, Log, Resource] r =>
  t Text ->
  ScratchOptions ->
  Sem r ScratchState
showInScratch lines' options = do
  scratch <- ensureScratch options
  scratch <$ setScratchContent scratch lines'

showInScratchDef ::
  Foldable t =>
  Members [Rpc !! RpcError, Rpc, AtomicState (Map ScratchId ScratchState), Reader PluginName, Log, Resource] r =>
  t Text ->
  Sem r ScratchState
showInScratchDef lines' =
  showInScratch lines' def

killScratch ::
  Members [Rpc !! RpcError, AtomicState (Map ScratchId ScratchState), Log] r =>
  ScratchState ->
  Sem r ()
killScratch (ScratchState name _ buffer window _ tab) = do
  Log.debug [exon|Killing scratch buffer '#{unScratchId name}'|]
  atomicModify' (Map.delete @_ @ScratchState name)
  resume_ @RpcError @Rpc do
      number <- bufferGetNumber buffer
      vimCommand [exon|autocmd! RibosomeScratch BufDelete <buffer=#{show number}>|]
  traverse_ (resume_ @RpcError @Rpc . closeTabpage) tab
  resume_ @RpcError @Rpc (closeWindow window)
  resume_ @RpcError @Rpc (wipeBuffer buffer)

scratchPreviousWindow ::
  Member (AtomicState (Map ScratchId ScratchState)) r =>
  ScratchId ->
  Sem r (Maybe Window)
scratchPreviousWindow =
  fmap (fmap ScratchState.previous) . lookupScratch

scratchWindow ::
  Member (AtomicState (Map ScratchId ScratchState)) r =>
  ScratchId ->
  Sem r (Maybe Window)
scratchWindow =
  fmap (fmap ScratchState.window) . lookupScratch

scratchBuffer ::
  Member (AtomicState (Map ScratchId ScratchState)) r =>
  ScratchId ->
  Sem r (Maybe Buffer)
scratchBuffer =
  fmap (fmap ScratchState.buffer) . lookupScratch
