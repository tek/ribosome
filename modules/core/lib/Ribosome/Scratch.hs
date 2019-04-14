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
import Ribosome.Control.Monad.Ribo (MonadRibo, NvimE, pluginInternalL, pluginModifyInternal)
import Ribosome.Control.Ribosome (RibosomeInternal)
import qualified Ribosome.Control.Ribosome as Ribosome (scratch)
import Ribosome.Data.Scratch (Scratch(Scratch))
import Ribosome.Data.ScratchOptions (ScratchOptions(ScratchOptions))
import qualified Ribosome.Data.ScratchOptions as ScratchOptions (ScratchOptions(name))
import Ribosome.Mapping (activateBufferMapping)
import Ribosome.Msgpack.Encode (toMsgpack)
import Ribosome.Msgpack.Error (DecodeError)
import Ribosome.Nvim.Api.Data (Buffer, Tabpage, Window)
import Ribosome.Nvim.Api.IO (
  bufferSetName,
  bufferSetOption,
  vimCommand,
  vimGetCurrentTabpage,
  vimGetCurrentWindow,
  vimSetCurrentWindow,
  windowGetBuffer,
  windowIsValid,
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
createScratchUi (ScratchOptions False vertical wrap focus size _ _ _) =
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
setupScratchIn previous window tab (ScratchOptions useTab _ _ focus _ syntax mappings name) = do
  buffer <- setupScratchBuffer window name
  traverse_ (executeWindowSyntax window) syntax
  traverse_ (activateBufferMapping buffer) mappings
  unless (focus || useTab) $ vimSetCurrentWindow previous
  let scratch = Scratch name buffer window previous tab
  pluginModifyInternal $ Lens.set (scratchLens name) (Just scratch)
  return scratch

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
  Scratch ->
  t Text ->
  m ()
setScratchContent (Scratch _ buffer _ _ _) lines' = do
  bufferSetOption buffer "modifiable" (toMsgpack True)
  setBufferContent buffer (toList lines')
  bufferSetOption buffer "modifiable" (toMsgpack False)

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
  setScratchContent scratch lines'
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
  pluginModifyInternal $ Lens.set (scratchLens name) Nothing
  where
    kill (Scratch _ buffer window _ tab) =
      traverse_ closeTabpage tab *> closeWindow window *> wipeBuffer buffer

scratchPreviousWindow ::
  MonadRibo m =>
  Text ->
  m (Maybe Window)
scratchPreviousWindow =
  fmap win <$$> lookupScratch
  where
    win (Scratch _ _ _ previous _) = previous
