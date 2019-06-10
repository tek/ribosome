module Ribosome.Scratch where

import Control.Lens (Lens')
import qualified Control.Lens as Lens (at, set)
import Control.Monad (unless)
import Data.Default (Default(def))
import Data.Foldable (traverse_)
import qualified Data.Map.Strict as Map (empty)
import Data.MessagePack (Object)

import Ribosome.Api.Buffer (setBufferContent, wipeBuffer)
import Ribosome.Api.Syntax (executeWindowSyntax)
import Ribosome.Api.Tabpage (closeTabpage)
import Ribosome.Api.Window (closeWindow)
import Ribosome.Control.Monad.Ribo (MonadRibo, NvimE, pluginInternalL, pluginInternalModify, pluginName)
import Ribosome.Control.Ribosome (RibosomeInternal)
import qualified Ribosome.Control.Ribosome as Ribosome (scratch)
import Ribosome.Data.FloatOptions (FloatOptions)
import Ribosome.Data.Scratch (Scratch(Scratch))
import qualified Ribosome.Data.Scratch as Scratch (Scratch(scratchPrevious, scratchWindow))
import Ribosome.Data.ScratchOptions (ScratchOptions(ScratchOptions))
import qualified Ribosome.Data.ScratchOptions as ScratchOptions (ScratchOptions(name, resize, maxSize))
import Ribosome.Data.Text (capitalize)
import Ribosome.Mapping (activateBufferMapping)
import Ribosome.Msgpack.Decode (fromMsgpack)
import Ribosome.Msgpack.Encode (toMsgpack)
import Ribosome.Msgpack.Error (DecodeError)
import Ribosome.Nvim.Api.Data (Buffer, Tabpage, Window)
import Ribosome.Nvim.Api.IO (
  bufferGetNumber,
  bufferSetName,
  bufferSetOption,
  nvimCreateBuf,
  nvimOpenWin,
  nvimWinSetBuf,
  vimCommand,
  vimGetCurrentTabpage,
  vimGetCurrentWindow,
  vimSetCurrentWindow,
  windowGetBuffer,
  windowIsValid,
  windowSetHeight,
  windowSetOption,
  )
import Ribosome.Nvim.Api.RpcCall (RpcError)

createScratchTab :: NvimE e m => m Tabpage
createScratchTab = do
  vimCommand "tabnew"
  vimGetCurrentTabpage

createRegularWindow ::
  NvimE e m =>
  Buffer ->
  Window ->
  Bool ->
  Bool ->
  Bool ->
  Maybe Int ->
  m Window
createRegularWindow buffer previous vertical focus bottom size = do
  vimCommand $ prefix <> cmd
  win <- vimGetCurrentWindow
  nvimWinSetBuf win buffer
  when bottom (vimCommand "wincmd J")
  unless focus $ vimSetCurrentWindow previous
  return win
  where
    cmd = if vertical then "vnew" else "new"
    prefix = maybe "" show size

floatConfig ::
  FloatOptions ->
  Map Text Object
floatConfig =
  fromRight Map.empty . fromMsgpack . toMsgpack

createFloat ::
  NvimE e m =>
  Bool ->
  FloatOptions ->
  Buffer ->
  m Window
createFloat focus options buffer =
  nvimOpenWin buffer focus (floatConfig options)

createScratchWindow ::
  NvimE e m =>
  Window ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  Maybe FloatOptions ->
  Maybe Int ->
  m (Buffer, Window)
createScratchWindow previous vertical wrap focus bottom float size = do
  buffer <- nvimCreateBuf True True
  win <- createWindow buffer
  windowSetOption win "wrap" (toMsgpack wrap)
  windowSetOption win "number" (toMsgpack False)
  windowSetOption win "cursorline" (toMsgpack True)
  return (buffer, win)
  where
    createWindow =
      maybe regular (createFloat focus) float
    regular buffer =
      createRegularWindow buffer previous vertical focus bottom size

createScratchUiInTab :: NvimE e m => m (Buffer, Window, Maybe Tabpage)
createScratchUiInTab = do
  tab <- createScratchTab
  win <- vimGetCurrentWindow
  buffer <- windowGetBuffer win
  return (buffer, win, Just tab)

createScratchUi ::
  NvimE e m =>
  Window ->
  ScratchOptions ->
  m (Buffer, Window, Maybe Tabpage)
createScratchUi previous (ScratchOptions False vertical wrap focus _ bottom float _ size _ _ _) =
  uncurry (,,Nothing) <$> createScratchWindow previous vertical wrap focus bottom float size
createScratchUi _ _ =
  createScratchUiInTab

configureScratchBuffer :: NvimE e m => Buffer -> Text -> m ()
configureScratchBuffer buffer name = do
  -- bufferSetOption buffer "buftype" (toMsgpack ("nofile" :: Text))
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

setupDeleteAutocmd ::
  MonadRibo m =>
  NvimE e m =>
  Scratch ->
  m ()
setupDeleteAutocmd (Scratch name buffer _ _ _) = do
  pname <- capitalize <$> pluginName
  number <- bufferGetNumber buffer
  vimCommand "augroup RibosomeScratch"
  vimCommand $ "autocmd RibosomeScratch BufDelete <buffer=" <> show number <> "> " <> deleteCall pname
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
setupScratchIn buffer previous window tab (ScratchOptions _ _ _ _ _ _ _ _ _ syntax mappings name) = do
  configureScratchBuffer buffer name
  traverse_ (executeWindowSyntax window) syntax
  traverse_ (activateBufferMapping buffer) mappings
  let scratch = Scratch name buffer window previous tab
  pluginInternalModify $ Lens.set (scratchLens name) (Just scratch)
  setupDeleteAutocmd scratch
  return scratch

createScratch ::
  MonadDeepError e DecodeError m =>
  MonadRibo m =>
  NvimE e m =>
  ScratchOptions ->
  m Scratch
createScratch options = do
  previous <- vimGetCurrentWindow
  (buffer, window, tab) <- createScratchUi previous options
  setupScratchIn buffer previous window tab options

updateScratch ::
  MonadDeepError e DecodeError m =>
  MonadRibo m =>
  NvimE e m =>
  Scratch ->
  ScratchOptions ->
  m Scratch
updateScratch (Scratch _ oldBuffer oldWindow _ oldTab) options = do
  previous <- vimGetCurrentWindow
  winValid <- windowIsValid oldWindow
  (buffer, window, tab) <- if winValid then return (oldBuffer, oldWindow, oldTab) else createScratchUi previous options
  setupScratchIn buffer previous window tab options

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
  when (ScratchOptions.resize options) (ignoreError @RpcError $ windowSetHeight win size)
  where
    size = max 1 $ min (length lines') (fromMaybe 30 (ScratchOptions.maxSize options))

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
  Scratch ->
  m ()
killScratch (Scratch name buffer window _ tab) = do
  catchAs @RpcError () removeAutocmd
  traverse_ closeTabpage tab *> closeWindow window *> wipeBuffer buffer
  pluginInternalModify $ Lens.set (scratchLens name) Nothing
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
