module Ribosome.Scratch where

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
import Ribosome.Data.Scratch (Scratch (Scratch))
import qualified Ribosome.Data.Scratch as Scratch (Scratch (scratchBuffer, scratchPrevious, scratchWindow))
import Ribosome.Data.ScratchOptions (ScratchOptions (ScratchOptions))
import qualified Ribosome.Data.ScratchOptions as ScratchOptions (maxSize, modify, name, resize, vertical)
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
  Text ->
  Sem r ()
configureScratchBuffer buffer ft name = do
  bufferSetOption buffer "bufhidden" (toMsgpack ("wipe" :: Text))
  bufferSetOption buffer "buftype" (toMsgpack ("nofile" :: Text))
  bufferSetOption buffer "swapfile" (toMsgpack False)
  traverse_ (bufferSetOption buffer "filetype" . toMsgpack) ft
  bufferSetName buffer name

setupScratchBuffer ::
  Members [Rpc, Log] r =>
  Window ->
  Buffer ->
  Maybe Text ->
  Text ->
  Sem r Buffer
setupScratchBuffer window buffer ft name = do
  valid <- nvimBufIsLoaded buffer
  Log.debug [exon|#{if valid then "" else "in"}valid scratch buffer|]
  validBuffer <- if valid then pure buffer else windowGetBuffer window
  configureScratchBuffer validBuffer ft name
  pure validBuffer

setupDeleteAutocmd ::
  Members [Rpc, Reader PluginName] r =>
  Scratch ->
  Sem r ()
setupDeleteAutocmd (Scratch name buffer _ _ _) = do
  PluginName pname <- pluginNameCapitalized
  bufferAutocmd buffer "RibosomeScratch" "BufDelete" (deleteCall pname)
  where
    deleteCall pname =
      "silent! call " <> pname <> "DeleteScratch('" <> name <> "')"

setupScratchIn ::
  Members [Rpc, AtomicState (Map Text Scratch), Reader PluginName, Log] r =>
  Buffer ->
  Window ->
  Window ->
  Maybe Tabpage ->
  ScratchOptions ->
  Sem r Scratch
setupScratchIn buffer previous window tab (ScratchOptions _ _ _ focus _ _ _ _ _ _ syntax mappings ft name) = do
  validBuffer <- setupScratchBuffer window buffer ft name
  traverse_ (executeWindowSyntax window) syntax
  traverse_ (activateBufferMapping validBuffer) mappings
  unless focus $ vimSetCurrentWindow previous
  let scratch = Scratch name validBuffer window previous tab
  atomicModify' (Map.insert name scratch)
  setupDeleteAutocmd scratch
  pure scratch

createScratch ::
  Members [Rpc, AtomicState (Map Text Scratch), Reader PluginName, Log, Resource] r =>
  ScratchOptions ->
  Sem r Scratch
createScratch options = do
  Log.debug [exon|creating new scratch: #{show options}|]
  previous <- vimGetCurrentWindow
  (buffer, window, tab) <- eventignore $ createScratchUi options
  eventignore $ setupScratchIn buffer previous window tab options

bufferStillLoaded ::
  Members [Rpc !! RpcError, Rpc] r =>
  Text ->
  Buffer ->
  Sem r Bool
bufferStillLoaded name buffer =
  (&&) <$> loaded <*> loadedName
  where
    loaded =
      nvimBufIsLoaded buffer
    loadedName =
      resumeAs @RpcError False ((name ==) <$> bufferGetName buffer)

updateScratch ::
  Members [Rpc !! RpcError, Rpc, AtomicState (Map Text Scratch), Reader PluginName, Log, Resource] r =>
  Scratch ->
  ScratchOptions ->
  Sem r Scratch
updateScratch oldScratch@(Scratch name oldBuffer oldWindow _ _) options = do
  Log.debug [exon|updating existing scratch '#{name}'|]
  ifM (windowIsValid oldWindow) attemptReuseWindow reset
  where
    attemptReuseWindow =
      ifM (bufferStillLoaded name oldBuffer) (pure oldScratch) closeAndReset
    closeAndReset =
      closeWindow oldWindow *> reset
    reset =
      createScratch options

lookupScratch ::
  Member (AtomicState (Map Text Scratch)) r =>
  Text ->
  Sem r (Maybe Scratch)
lookupScratch name =
  atomicGets (Map.lookup name)

ensureScratch ::
  Members [Rpc !! RpcError, Rpc, AtomicState (Map Text Scratch), Reader PluginName, Log, Resource] r =>
  ScratchOptions ->
  Sem r Scratch
ensureScratch options = do
  f <- maybe createScratch updateScratch <$> lookupScratch (ScratchOptions.name options)
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
      ScratchOptions.modify options
    wrap =
      update True *> thunk <* update False
    update value =
      bufferSetOption buffer "modifiable" (toMsgpack value)

setScratchContent ::
  Foldable t =>
  Members [Rpc !! RpcError, Rpc] r =>
  ScratchOptions ->
  Scratch ->
  t Text ->
  Sem r ()
setScratchContent options (Scratch _ buffer win _ _) lines' = do
  withModifiable buffer options $ setBufferContent buffer (toList lines')
  when (ScratchOptions.resize options) (resume_ @RpcError @Rpc (setSize win size))
  where
    size =
      max 1 calculateSize
    calculateSize =
      if vertical then fromMaybe 50 maxSize else min (length lines') (fromMaybe 30 maxSize)
    maxSize =
      ScratchOptions.maxSize options
    vertical =
      ScratchOptions.vertical options
    setSize =
      if vertical then windowSetWidth else windowSetHeight

showInScratch ::
  Foldable t =>
  Members [Rpc !! RpcError, Rpc, AtomicState (Map Text Scratch), Reader PluginName, Log, Resource] r =>
  t Text ->
  ScratchOptions ->
  Sem r Scratch
showInScratch lines' options = do
  scratch <- ensureScratch options
  scratch <$ setScratchContent options scratch lines'

showInScratchDef ::
  Foldable t =>
  Members [Rpc !! RpcError, Rpc, AtomicState (Map Text Scratch), Reader PluginName, Log, Resource] r =>
  t Text ->
  Sem r Scratch
showInScratchDef lines' =
  showInScratch lines' def

killScratch ::
  Members [Rpc !! RpcError, AtomicState (Map Text Scratch), Log] r =>
  Scratch ->
  Sem r ()
killScratch (Scratch name buffer window _ tab) = do
  Log.debug [exon|Killing scratch buffer '#{name}'|]
  atomicModify' (Map.delete @_ @Scratch name)
  resume_ @RpcError @Rpc do
      number <- bufferGetNumber buffer
      vimCommand [exon|autocmd! RibosomeScratch BufDelete <buffer=#{show number}>|]
  traverse_ (resume_ @RpcError @Rpc . closeTabpage) tab
  resume_ @RpcError @Rpc (closeWindow window)
  resume_ @RpcError @Rpc (wipeBuffer buffer)

killScratchByName ::
  Members [Rpc !! RpcError, AtomicState (Map Text Scratch), Log] r =>
  Text ->
  Sem r ()
killScratchByName =
  traverse_ killScratch <=< lookupScratch

scratchPreviousWindow ::
  Member (AtomicState (Map Text Scratch)) r =>
  Text ->
  Sem r (Maybe Window)
scratchPreviousWindow =
  fmap (fmap Scratch.scratchPrevious) <$> lookupScratch

scratchWindow ::
  Member (AtomicState (Map Text Scratch)) r =>
  Text ->
  Sem r (Maybe Window)
scratchWindow =
  fmap (fmap Scratch.scratchWindow) <$> lookupScratch

scratchBuffer ::
  Member (AtomicState (Map Text Scratch)) r =>
  Text ->
  Sem r (Maybe Buffer)
scratchBuffer =
  fmap (fmap Scratch.scratchBuffer) <$> lookupScratch
