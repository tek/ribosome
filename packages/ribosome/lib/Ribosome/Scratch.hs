module Ribosome.Scratch where

import Control.Lens (Lens', set, view)
import qualified Control.Lens as Lens (at)
import qualified Data.Map.Strict as Map (empty)
import Data.MessagePack (Object)

import Ribosome.Api.Autocmd (bufferAutocmd, eventignore)
import Ribosome.Api.Buffer (setBufferContent, wipeBuffer)
import Ribosome.Api.Syntax (executeCurrentWindowSyntax)
import Ribosome.Api.Tabpage (closeTabpage)
import Ribosome.Api.Window (closeWindow)
import Ribosome.Control.Monad.Ribo (MonadRibo, NvimE, pluginInternalL, pluginInternalModify, pluginName)
import Ribosome.Control.Ribosome (RibosomeInternal)
import qualified Ribosome.Control.Ribosome as Ribosome (scratch)
import Ribosome.Data.FloatOptions (FloatOptions)
import Ribosome.Data.Scratch (Scratch(Scratch))
import qualified Ribosome.Data.Scratch as Scratch (Scratch(scratchPrevious, scratchWindow, scratchBuffer))
import Ribosome.Data.ScratchOptions (ScratchOptions(ScratchOptions))
import qualified Ribosome.Data.ScratchOptions as ScratchOptions (maxSize, modify, name, resize, vertical)
import Ribosome.Data.Text (capitalize)
import Ribosome.Log (logDebug)
import Ribosome.Mapping (activateBufferMapping)
import Ribosome.Msgpack.Decode (fromMsgpack)
import Ribosome.Msgpack.Encode (toMsgpack)
import Ribosome.Msgpack.Error (DecodeError)
import Ribosome.Nvim.Api.Data (Buffer, Tabpage, Window)
import Ribosome.Nvim.Api.IO (
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
import Ribosome.Nvim.Api.RpcCall (RpcError)

createScratchTab :: NvimE e m => m Tabpage
createScratchTab = do
  vimCommand "tabnew"
  vimGetCurrentTabpage

createRegularWindow ::
  NvimE e m =>
  Bool ->
  Bool ->
  Maybe Int ->
  m (Buffer, Window)
createRegularWindow vertical bottom size = do
  vimCommand prefixedCmd
  buf <- vimGetCurrentBuffer
  win <- vimGetCurrentWindow
  return (buf, win)
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

createFloat ::
  NvimE e m =>
  FloatOptions ->
  m (Buffer, Window)
createFloat options = do
  buffer <- nvimCreateBuf True True
  window <- nvimOpenWin buffer True (floatConfig options)
  return (buffer, window)

createScratchWindow ::
  NvimE e m =>
  Bool ->
  Bool ->
  Bool ->
  Maybe FloatOptions ->
  Maybe Int ->
  m (Buffer, Window)
createScratchWindow vertical wrap bottom float size = do
  (buffer, win) <- createWindow
  windowSetOption win "wrap" (toMsgpack wrap)
  windowSetOption win "number" (toMsgpack False)
  windowSetOption win "cursorline" (toMsgpack True)
  windowSetOption win "colorcolumn" (toMsgpack ("" :: Text))
  windowSetOption win "foldmethod" (toMsgpack ("manual" :: Text))
  windowSetOption win "conceallevel" (toMsgpack (2 :: Int))
  return (buffer, win)
  where
    createWindow =
      maybe regular createFloat float
    regular =
      createRegularWindow vertical bottom size

createScratchUiInTab :: NvimE e m => m (Buffer, Window, Maybe Tabpage)
createScratchUiInTab = do
  tab <- createScratchTab
  win <- vimGetCurrentWindow
  buffer <- windowGetBuffer win
  return (buffer, win, Just tab)

createScratchUi ::
  NvimE e m =>
  ScratchOptions ->
  m (Buffer, Window, Maybe Tabpage)
createScratchUi (ScratchOptions False vertical wrap _ _ bottom _ float size _ _ _ _) =
  uncurry (,,Nothing) <$> createScratchWindow vertical wrap bottom float size
createScratchUi _ =
  createScratchUiInTab

configureScratchBuffer :: NvimE e m => Buffer -> Text -> m ()
configureScratchBuffer buffer name = do
  bufferSetOption buffer "bufhidden" (toMsgpack ("wipe" :: Text))
  bufferSetOption buffer "buftype" (toMsgpack ("nofile" :: Text))
  bufferSetOption buffer "swapfile" (toMsgpack False)
  bufferSetName buffer name

setupScratchBuffer ::
  NvimE e m =>
  MonadRibo m =>
  Window ->
  Buffer ->
  Text ->
  m Buffer
setupScratchBuffer window buffer name = do
  valid <- nvimBufIsLoaded buffer
  logDebug @Text $ (if valid then "" else "in") <> "valid scratch buffer"
  validBuffer <- if valid then return buffer else windowGetBuffer window
  configureScratchBuffer validBuffer name
  return validBuffer

scratchLens :: Text -> Lens' RibosomeInternal (Maybe Scratch)
scratchLens name =
  Ribosome.scratch . Lens.at name

setupDeleteAutocmd ::
  MonadRibo m =>
  NvimE e m =>
  Scratch ->
  m ()
setupDeleteAutocmd (Scratch name buffer _ _ _) = do
  pname <- capitalize <$> pluginName
  bufferAutocmd buffer "RibosomeScratch" "BufDelete" (deleteCall pname)
  where
    deleteCall pname =
      "silent! call " <> pname <> "DeleteScratch('" <> name <> "')"

setupScratchIn ::
  MonadDeepError e DecodeError m =>
  MonadRibo m =>
  NvimE e m =>
  Buffer ->
  Window ->
  Window ->
  Maybe Tabpage ->
  ScratchOptions ->
  m Scratch
setupScratchIn buffer previous window tab (ScratchOptions _ _ _ focus _ _ _ _ _ _ syntax mappings name) = do
  validBuffer <- setupScratchBuffer window buffer name
  traverse_ executeCurrentWindowSyntax syntax
  traverse_ (activateBufferMapping validBuffer) mappings
  unless focus $ vimSetCurrentWindow previous
  let scratch = Scratch name validBuffer window previous tab
  pluginInternalModify $ set (scratchLens name) (Just scratch)
  setupDeleteAutocmd scratch
  return scratch

createScratch ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepError e DecodeError m =>
  ScratchOptions ->
  m Scratch
createScratch options = do
  logDebug @Text $ "creating new scratch `" <> show options <> "`"
  previous <- vimGetCurrentWindow
  (buffer, window, tab) <- eventignore $ createScratchUi options
  eventignore $ setupScratchIn buffer previous window tab options

bufferStillLoaded ::
  NvimE e m =>
  Text ->
  Buffer ->
  m Bool
bufferStillLoaded name buffer =
  (&&) <$> loaded <*> loadedName
  where
    loaded = nvimBufIsLoaded buffer
    loadedName = catchAs @RpcError False ((name ==) <$> bufferGetName buffer)

updateScratch ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepError e DecodeError m =>
  Scratch ->
  ScratchOptions ->
  m Scratch
updateScratch oldScratch@(Scratch name oldBuffer oldWindow _ _) options = do
  logDebug $ "updating existing scratch `" <> name <> "`"
  ifM (windowIsValid oldWindow) attemptReuseWindow reset
  where
    attemptReuseWindow =
      ifM (bufferStillLoaded name oldBuffer) (return oldScratch) closeAndReset
    closeAndReset =
      closeWindow oldWindow *> reset
    reset =
      createScratch options

lookupScratch ::
  MonadRibo m =>
  Text ->
  m (Maybe Scratch)
lookupScratch name =
  pluginInternalL (scratchLens name)

ensureScratch ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepError e DecodeError m =>
  ScratchOptions ->
  m Scratch
ensureScratch options = do
  f <- maybe createScratch updateScratch <$> lookupScratch (view ScratchOptions.name options)
  f options

withModifiable ::
  NvimE e m =>
  Buffer ->
  ScratchOptions ->
  m a ->
  m a
withModifiable buffer options thunk =
  if isWrite then thunk else wrap
  where
    isWrite =
      view ScratchOptions.modify options
    wrap =
      update True *> thunk <* update False
    update value =
      bufferSetOption buffer "modifiable" (toMsgpack value)

setScratchContent ::
  Foldable t =>
  NvimE e m =>
  ScratchOptions ->
  Scratch ->
  t Text ->
  m ()
setScratchContent options (Scratch _ buffer win _ _) lines' = do
  withModifiable buffer options $ setBufferContent buffer (toList lines')
  when (view ScratchOptions.resize options) (ignoreError @RpcError $ setSize win size)
  where
    size =
      max 1 calculateSize
    calculateSize =
      if vertical then fromMaybe 50 maxSize else min (length lines') (fromMaybe 30 maxSize)
    maxSize =
      view ScratchOptions.maxSize options
    vertical =
      view ScratchOptions.vertical options
    setSize =
      if vertical then windowSetWidth else windowSetHeight

showInScratch ::
  Foldable t =>
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepError e DecodeError m =>
  t Text ->
  ScratchOptions ->
  m Scratch
showInScratch lines' options = do
  scratch <- ensureScratch options
  setScratchContent options scratch lines'
  return scratch

showInScratchDef ::
  Foldable t =>
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepError e DecodeError m =>
  t Text ->
  m Scratch
showInScratchDef lines' =
  showInScratch lines' def

killScratch ::
  MonadRibo m =>
  NvimE e m =>
  Scratch ->
  m ()
killScratch (Scratch name buffer window _ tab) = do
  catchAs @RpcError () removeAutocmd
  traverse_ closeTabpage tab *> closeWindow window *> wipeBuffer buffer
  pluginInternalModify $ set (scratchLens name) Nothing
  where
    removeAutocmd = do
      number <- bufferGetNumber buffer
      vimCommand $ "autocmd! RibosomeScratch BufDelete <buffer=" <> show number <> ">"

killScratchByName ::
  MonadRibo m =>
  NvimE e m =>
  Text ->
  m ()
killScratchByName =
  traverse_ killScratch <=< lookupScratch

scratchPreviousWindow ::
  MonadRibo m =>
  Text ->
  m (Maybe Window)
scratchPreviousWindow =
  fmap Scratch.scratchPrevious <$$> lookupScratch

scratchWindow ::
  MonadRibo m =>
  Text ->
  m (Maybe Window)
scratchWindow =
  fmap Scratch.scratchWindow <$$> lookupScratch

scratchBuffer ::
  MonadRibo m =>
  Text ->
  m (Maybe Buffer)
scratchBuffer =
  fmap Scratch.scratchBuffer <$$> lookupScratch
