{-# options_haddock prune #-}

-- |Internal logic for 'Ribosome.Scratch'.
module Ribosome.Internal.Scratch where

import qualified Data.Map.Strict as Map
import Data.MessagePack (Object)
import Exon (exon)
import qualified Polysemy.Log as Log
import Prelude hiding (group)

import Ribosome.Api.Autocmd (bufferAutocmd, eventignore)
import Ribosome.Api.Buffer (setBufferContent, wipeBuffer)
import Ribosome.Api.Syntax (executeWindowSyntax)
import Ribosome.Api.Tabpage (closeTabpage)
import Ribosome.Api.Window (closeWindow)
import Ribosome.Data.FloatOptions (FloatOptions, enter)
import Ribosome.Data.PluginName (PluginName (PluginName))
import Ribosome.Data.ScratchId (ScratchId (ScratchId))
import Ribosome.Data.ScratchOptions (ScratchOptions (ScratchOptions, filetype, name), focus, mappings, syntax)
import qualified Ribosome.Data.ScratchState as ScratchState
import Ribosome.Data.ScratchState (ScratchState (ScratchState))
import Ribosome.Host.Api.Data (
  Buffer,
  Tabpage,
  Window,
  bufferGetName,
  bufferSetName,
  bufferSetOption,
  nvimBufIsLoaded,
  nvimCreateBuf,
  nvimDelAutocmd,
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
import Ribosome.Host.Class.MonadRpc (MonadRpc (atomic))
import Ribosome.Host.Class.Msgpack.Decode (fromMsgpack)
import Ribosome.Host.Class.Msgpack.Encode (toMsgpack)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Data.RpcType (AutocmdId (AutocmdId), group)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Mapping (activateBufferMappings)
import Ribosome.PluginName (pluginNamePascalCase)

createScratchTab ::
  MonadRpc m =>
  m Tabpage
createScratchTab = do
  vimCommand "tabnew"
  vimGetCurrentTabpage

createRegularWindow ::
  MonadRpc m =>
  Bool ->
  Bool ->
  Maybe Int ->
  m (Buffer, Window)
createRegularWindow vertical bottom size = do
  vimCommand prefixedCmd
  buf <- vimGetCurrentBuffer
  win <- vimGetCurrentWindow
  pure (buf, win)
  where
    prefixedCmd =
      [exon|#{locationPrefix} #{sizePrefix}#{cmd}|]
    cmd =
      if vertical then "vnew" else "new"
    sizePrefix =
      foldMap show size
    locationPrefix =
      if bottom then "belowright" else "aboveleft"

floatConfig ::
  FloatOptions ->
  Map Text Object
floatConfig =
  fromRight Map.empty . fromMsgpack . toMsgpack

createFloatWith ::
  MonadRpc m =>
  Bool ->
  Bool ->
  FloatOptions ->
  m (Buffer, Window)
createFloatWith listed scratch options = do
  buffer <- nvimCreateBuf listed scratch
  window <- nvimOpenWin buffer options.enter (floatConfig options)
  pure (buffer, window)

createFloat ::
  MonadRpc m =>
  FloatOptions ->
  m (Buffer, Window)
createFloat =
  createFloatWith True True

createScratchWindow ::
  MonadRpc m =>
  Bool ->
  Bool ->
  Bool ->
  Maybe FloatOptions ->
  Maybe Int ->
  m (Buffer, Window)
createScratchWindow vertical wrap bottom float size =
  atomic do
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

createScratchUiInTab ::
  MonadRpc m =>
  m (Buffer, Window, Maybe Tabpage)
createScratchUiInTab = do
  tab <- createScratchTab
  win <- vimGetCurrentWindow
  buffer <- windowGetBuffer win
  pure (buffer, win, Just tab)

createScratchUi ::
  MonadRpc m =>
  ScratchOptions ->
  m (Buffer, Window, Maybe Tabpage)
createScratchUi (ScratchOptions False vertical wrap _ _ bottom _ float size _ _ _ _ _) =
  uncurry (,,Nothing) <$> createScratchWindow vertical wrap bottom float size
createScratchUi _ =
  createScratchUiInTab

configureScratchBuffer ::
  MonadRpc m =>
  Buffer ->
  Maybe Text ->
  ScratchId ->
  m ()
configureScratchBuffer buffer ft (ScratchId name) = do
  atomic do
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
  ScratchId ->
  Buffer ->
  Sem r AutocmdId
setupDeleteAutocmd (ScratchId name) buffer = do
  PluginName pname <- pluginNamePascalCase
  bufferAutocmd buffer "BufDelete" def { group = Just "RibosomeScratch" } (deleteCall pname)
  where
    deleteCall pname =
      [exon|silent! call #{pname}DeleteScratch('#{name}')|]

setupScratchIn ::
  Members [Rpc, AtomicState (Map ScratchId ScratchState), Reader PluginName, Log] r =>
  Buffer ->
  Window ->
  Window ->
  Maybe Tabpage ->
  ScratchOptions ->
  Sem r ScratchState
setupScratchIn buffer previous window tab options@(ScratchOptions {..}) = do
  validBuffer <- setupScratchBuffer window buffer filetype name
  traverse_ (executeWindowSyntax window) syntax
  activateBufferMappings validBuffer mappings
  unless focus (vimSetCurrentWindow previous)
  auId <- setupDeleteAutocmd name validBuffer
  let scratch = ScratchState name options validBuffer window previous tab auId
  atomicModify' (Map.insert name scratch)
  pure scratch

createScratch ::
  Members [Rpc, AtomicState (Map ScratchId ScratchState), Reader PluginName, Log, Resource] r =>
  ScratchOptions ->
  Sem r ScratchState
createScratch options = do
  Log.debug [exon|creating new scratch: #{show options}|]
  previous <- vimGetCurrentWindow
  (buffer, window, tab) <- eventignore (createScratchUi options)
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
updateScratch oldScratch@(ScratchState name _ oldBuffer oldWindow _ _ _) options = do
  Log.debug [exon|updating existing scratch `#{coerce name}`|]
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
setScratchContent (ScratchState _ options buffer win _ _ _) lines' = do
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
killScratch (ScratchState name _ buffer window _ tab (AutocmdId auId)) = do
  Log.debug [exon|Killing scratch buffer `##{name}`|]
  atomicModify' (Map.delete @_ @ScratchState name)
  resume_ (nvimDelAutocmd auId)
  traverse_ (resume_ . closeTabpage) tab
  resume_ (closeWindow window)
  resume_ (wipeBuffer buffer)

scratchPreviousWindow ::
  Member (AtomicState (Map ScratchId ScratchState)) r =>
  ScratchId ->
  Sem r (Maybe Window)
scratchPreviousWindow =
  fmap (fmap (.previous)) . lookupScratch

scratchWindow ::
  Member (AtomicState (Map ScratchId ScratchState)) r =>
  ScratchId ->
  Sem r (Maybe Window)
scratchWindow =
  fmap (fmap (.window)) . lookupScratch

scratchBuffer ::
  Member (AtomicState (Map ScratchId ScratchState)) r =>
  ScratchId ->
  Sem r (Maybe Buffer)
scratchBuffer =
  fmap (fmap (.buffer)) . lookupScratch
