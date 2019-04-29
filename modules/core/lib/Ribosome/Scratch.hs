module Ribosome.Scratch where

import Control.Lens (Lens')
import qualified Control.Lens as Lens (at, set)
import Control.Monad (unless)
import Data.Default (Default(def))
import Data.Foldable (traverse_)

import Ribosome.Api.Buffer (setBufferContent, wipeBuffer)
import Ribosome.Api.Syntax (executeWindowSyntax)
import Ribosome.Api.Tabpage (closeTabpage)
import Ribosome.Api.Window (closeWindow)
import Ribosome.Control.Monad.Ribo (MonadRibo, NvimE, pluginInternalL, pluginInternalModify, pluginName)
import Ribosome.Control.Ribosome (RibosomeInternal)
import qualified Ribosome.Control.Ribosome as Ribosome (scratch)
import Ribosome.Data.Scratch (Scratch(Scratch))
import qualified Ribosome.Data.Scratch as Scratch (Scratch(scratchPrevious, scratchWindow))
import Ribosome.Data.ScratchOptions (ScratchOptions(ScratchOptions))
import qualified Ribosome.Data.ScratchOptions as ScratchOptions (ScratchOptions(name, resize, maxSize))
import Ribosome.Data.Text (capitalize)
import Ribosome.Mapping (activateBufferMapping)
import Ribosome.Msgpack.Encode (toMsgpack)
import Ribosome.Msgpack.Error (DecodeError)
import Ribosome.Nvim.Api.Data (Buffer, Tabpage, Window)
import Ribosome.Nvim.Api.IO (
  bufferGetNumber,
  bufferSetName,
  bufferSetOption,
  vimCommand,
  vimGetCurrentTabpage,
  vimGetCurrentWindow,
  vimSetCurrentWindow,
  windowGetBuffer,
  windowIsValid,
  windowSetHeight,
  windowSetOption,
  )

createScratchTab :: NvimE e m => m Tabpage
createScratchTab = do
  vimCommand "tabnew"
  vimGetCurrentTabpage

createScratchWindow :: NvimE e m => Bool -> Bool -> Maybe Int -> m Window
createScratchWindow vertical wrap size = do
  vimCommand $ prefix <> cmd
  win <- vimGetCurrentWindow
  windowSetOption win "wrap" (toMsgpack wrap)
  return win
  where
    cmd = if vertical then "vnew" else "new"
    prefix = maybe "" show size

createScratchUiInTab :: NvimE e m => m (Window, Maybe Tabpage)
createScratchUiInTab = do
  tab <- createScratchTab
  win <- vimGetCurrentWindow
  return (win, Just tab)

createScratchUiInWindow :: NvimE e m => Bool -> Bool -> Bool -> Maybe Int -> m (Window, Maybe Tabpage)
createScratchUiInWindow vertical wrap _ size = do
  win <- createScratchWindow vertical wrap size
  return (win, Nothing)

createScratchUi :: NvimE e m => ScratchOptions -> m (Window, Maybe Tabpage)
createScratchUi (ScratchOptions False vertical wrap focus _ _ size _ _ _) =
  createScratchUiInWindow vertical wrap focus size
createScratchUi _ =
  createScratchUiInTab

configureScratchBuffer :: NvimE e m => Buffer -> Text -> m ()
configureScratchBuffer buffer name = do
  bufferSetOption buffer "buftype" (toMsgpack ("nofile" :: Text))
  bufferSetOption buffer "bufhidden" (toMsgpack ("wipe" :: Text))
  bufferSetName buffer name

setupScratchBuffer :: NvimE e m => Window -> Text -> m Buffer
setupScratchBuffer window name = do
  buffer <- windowGetBuffer window
  configureScratchBuffer buffer name
  return buffer

scratchLens :: Text -> Lens' RibosomeInternal (Maybe Scratch)
scratchLens name =
  Ribosome.scratch . Lens.at name

setupScratchIn ::
  MonadDeepError e DecodeError m =>
  MonadRibo m =>
  NvimE e m =>
  Window ->
  Window ->
  Maybe Tabpage ->
  ScratchOptions ->
  m Scratch
setupScratchIn previous window tab (ScratchOptions useTab _ _ focus _ _ _ syntax mappings name) = do
  buffer <- setupScratchBuffer window name
  traverse_ (executeWindowSyntax window) syntax
  traverse_ (activateBufferMapping buffer) mappings
  unless (focus || useTab) $ vimSetCurrentWindow previous
  let scratch = Scratch name buffer window previous tab
  pluginInternalModify $ Lens.set (scratchLens name) (Just scratch)
  setupDeleteAutocmd scratch
  return scratch

setupDeleteAutocmd ::
  MonadRibo m =>
  NvimE e m =>
  Scratch ->
  m ()
setupDeleteAutocmd (Scratch name buffer _ _ _) = do
  pname <- capitalize <$> pluginName
  number <- bufferGetNumber buffer
  vimCommand $ "autocmd BufDelete <buffer=" <> show number <> "> silent! call " <> pname <> "DeleteScratch('" <> name <> "')"

createScratch ::
  MonadDeepError e DecodeError m =>
  MonadRibo m =>
  NvimE e m =>
  ScratchOptions ->
  m Scratch
createScratch options = do
  previous <- vimGetCurrentWindow
  (window, tab) <- createScratchUi options
  setupScratchIn previous window tab options

updateScratch ::
  MonadDeepError e DecodeError m =>
  MonadRibo m =>
  NvimE e m =>
  Scratch ->
  ScratchOptions ->
  m Scratch
updateScratch (Scratch _ _ oldWindow _ oldTab) options = do
  previous <- vimGetCurrentWindow
  winValid <- windowIsValid oldWindow
  (window, tab) <- if winValid then return (oldWindow, oldTab) else createScratchUi options
  setupScratchIn previous window tab options

lookupScratch ::
  MonadRibo m =>
  Text ->
  m (Maybe Scratch)
lookupScratch name =
  pluginInternalL (scratchLens name)

ensureScratch ::
  MonadDeepError e DecodeError m =>
  MonadRibo m =>
  NvimE e m =>
  ScratchOptions ->
  m Scratch
ensureScratch options = do
  f <- maybe createScratch updateScratch <$> lookupScratch (ScratchOptions.name options)
  f options

setScratchContent ::
  Foldable t =>
  NvimE e m =>
  ScratchOptions ->
  Scratch ->
  t Text ->
  m ()
setScratchContent options (Scratch _ buffer win _ _) lines' = do
  bufferSetOption buffer "modifiable" (toMsgpack True)
  setBufferContent buffer (toList lines')
  bufferSetOption buffer "modifiable" (toMsgpack False)
  when (ScratchOptions.resize options) (windowSetHeight win size)
  where
    size = min (length lines') (fromMaybe 30 (ScratchOptions.maxSize options))

showInScratch ::
  Foldable t =>
  MonadDeepError e DecodeError m =>
  MonadRibo m =>
  NvimE e m =>
  t Text ->
  ScratchOptions ->
  m Scratch
showInScratch lines' options = do
  scratch <- ensureScratch options
  setScratchContent options scratch lines'
  return scratch

showInScratchDef ::
  Foldable t =>
  MonadDeepError e DecodeError m =>
  MonadRibo m =>
  NvimE e m =>
  t Text ->
  m Scratch
showInScratchDef lines' =
  showInScratch lines' def

killScratch ::
  MonadRibo m =>
  NvimE e m =>
  Text ->
  m ()
killScratch name = do
  maybe (return ()) kill =<< lookupScratch name
  pluginInternalModify $ Lens.set (scratchLens name) Nothing
  where
    kill (Scratch _ buffer window _ tab) =
      traverse_ closeTabpage tab *> closeWindow window *> wipeBuffer buffer

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
