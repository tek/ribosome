-- |API functions for buffers.
module Ribosome.Api.Buffer where

import qualified Data.Text as Text (null)
import Exon (exon)
import Path (Abs, Dir, File, Path, parseAbsFile, parseRelFile, (</>))

import Ribosome.Api.Path (nvimCwd)
import Ribosome.Data.FileBuffer (FileBuffer (FileBuffer))
import Ribosome.Host.Api.Data (
  Buffer,
  bufferGetLines,
  bufferGetName,
  bufferGetNumber,
  bufferGetOption,
  bufferIsValid,
  bufferSetLines,
  nvimBufDelete,
  nvimCallFunction,
  nvimCommand,
  nvimGetCurrentBuf,
  nvimWinSetBuf,
  vimGetBuffers,
  vimGetCurrentBuffer,
  vimGetCurrentWindow,
  )
import Ribosome.Host.Class.MonadRpc (MonadRpc, atomic)
import Ribosome.Host.Class.Msgpack.Encode (toMsgpack)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Modify (silentBang)
import Ribosome.Host.Path (pathText)

-- |Load a 'Path' into a buffer in the current window using @:edit@.
edit ::
  Member Rpc r =>
  Path b t ->
  Sem r ()
edit path =
  silentBang do
    nvimCommand [exon|edit #{pathText path}|]

-- |Call the Neovim function @buflisted@ for a buffer, indicating whether it is shown in the buffer list (@:ls@).
buflisted ::
  Member (Rpc !! RpcError) r =>
  Buffer ->
  Sem r Bool
buflisted buf = do
  resumeAs False do
    num <- bufferGetNumber buf
    nvimCallFunction "buflisted" [toMsgpack num]

-- |Return the entire content of the given buffer.
bufferContent ::
  MonadRpc m =>
  Buffer ->
  m [Text]
bufferContent buffer =
  bufferGetLines buffer 0 (-1) False

-- |Return the entire content of the current buffer.
currentBufferContent ::
  MonadRpc m =>
  m [Text]
currentBufferContent =
  bufferContent =<< vimGetCurrentBuffer

-- |Replace the content of the given buffer.
setBufferContent ::
  MonadRpc m =>
  Buffer ->
  [Text] ->
  m ()
setBufferContent buffer =
  bufferSetLines buffer 0 (-1) False

-- |Replace the content of the current buffer.
setCurrentBufferContent ::
  MonadRpc m =>
  [Text] ->
  m ()
setCurrentBufferContent content = do
  buffer <- vimGetCurrentBuffer
  setBufferContent buffer content

-- |Replace a single line in the given buffer.
setBufferLine ::
  MonadRpc m =>
  Buffer ->
  Int ->
  Text ->
  m ()
setBufferLine buffer line text =
  bufferSetLines buffer line (line + 1) False [text]

-- |Execute an action only if the given buffer is valid, i.e. it exists but may be unloaded.
whenValid ::
  MonadRpc m =>
  (Buffer -> m ()) ->
  Buffer ->
  m ()
whenValid use buffer =
  whenM (bufferIsValid buffer) (use buffer)

-- |Execute an action with the given buffer's number if it is valid.
withBufferNumber ::
  MonadRpc m =>
  (Int -> m ()) ->
  Buffer ->
  m ()
withBufferNumber run =
 whenValid (run <=< bufferGetNumber)

-- |Force-delete a buffer, discarding changes.
closeBuffer ::
  Member Rpc r =>
  Buffer ->
  Sem r ()
closeBuffer =
  silentBang . withBufferNumber del
  where
    del number =
      nvimCommand [exon|bdelete! #{show number}|]

-- |Force-wipe a buffer, discarding changes.
wipeBuffer ::
  MonadRpc m =>
  Buffer ->
  m ()
wipeBuffer =
  whenValid \ b -> nvimBufDelete b [("force", toMsgpack True)]

-- |Force-unload a buffer, discarding changes.
unloadBuffer ::
  MonadRpc m =>
  Buffer ->
  m ()
unloadBuffer =
  whenValid \ b -> nvimBufDelete b [("force", toMsgpack True), ("unload", toMsgpack True)]

-- |Add a buffer to the list without loading it.
addBuffer ::
  MonadRpc m =>
  Text ->
  m ()
addBuffer path =
  nvimCommand [exon|badd #{path}|]

-- |Construct a file buffer from a path if it is parseable.
fileBuffer ::
  Path Abs Dir ->
  Buffer ->
  Text ->
  Maybe FileBuffer
fileBuffer cwd buffer (toString -> path) =
  FileBuffer buffer <$> (parseAbsFile path <|> (cwd </>) <$> parseRelFile path)

-- |Get all buffers in the buffer list whose name is a path.
fileBuffers ::
  MonadRpc m =>
  m [FileBuffer]
fileBuffers =
  atomic do
    (cwd, buffers) <- (,) <$> nvimCwd <*> vimGetBuffers
    names <- foldMap (fmap pure . bufferGetName) buffers
    pure (catMaybes (zipWith (fileBuffer cwd) buffers names))

-- |Find the buffer whose name is the given path.
bufferForFile ::
  MonadRpc m =>
  Path Abs File ->
  m (Maybe FileBuffer)
bufferForFile target =
  find sameBuffer <$> fileBuffers
  where
    sameBuffer (FileBuffer _ path) =
      path == target

-- |Return the name of the current buffer.
currentBufferName ::
  MonadRpc m =>
  m Text
currentBufferName =
  bufferGetName =<< vimGetCurrentBuffer

-- |Set the current buffer.
setCurrentBuffer ::
  MonadRpc m =>
  Buffer ->
  m ()
setCurrentBuffer buf = do
  win <- vimGetCurrentWindow
  nvimWinSetBuf win buf

-- |Indicate whether the given buffer is a file, i.e. has empty @buftype@.
bufferIsFile ::
  MonadRpc m =>
  Buffer ->
  m Bool
bufferIsFile buf =
  Text.null <$> bufferGetOption buf "buftype"

-- |Return the number of buffers in the list.
bufferCount ::
  MonadRpc m =>
  m Natural
bufferCount =
  fromIntegral . length <$> vimGetBuffers

-- |Return the file system path of the given buffer, if its name is a valid path.
bufferPath ::
  MonadRpc m =>
  Buffer ->
  m (Maybe (Path Abs File))
bufferPath buffer = do
  atomic do
    cwd <- nvimCallFunction "getcwd" []
    name <- bufferGetName buffer
    pure (fileBuffer cwd buffer name <&> \ (FileBuffer _ path) -> path)

-- |Return the file system path of the current buffer, if its name is a valid path.
currentBufferPath ::
  MonadRpc m =>
  m (Maybe (Path Abs File))
currentBufferPath =
  bufferPath =<< nvimGetCurrentBuf

-- |Filter out the unlisted buffers from the given list.
filterListed ::
  MonadRpc m =>
  [Buffer] ->
  m [Buffer]
filterListed bufs = do
  atomic do
    nums <- traverse bufferGetNumber bufs
    listedFlags <- traverse (nvimCallFunction "buflisted" . pure . toMsgpack) nums
    let listed = mapMaybe chooseListed (zip bufs listedFlags)
    buftypes :: [Text] <- traverse (flip bufferGetOption "buftype") listed
    pure (mapMaybe chooseEmptyTypes (zip bufs buftypes))
  where
    chooseListed (b, l) =
      if l then Just b else Nothing
    chooseEmptyTypes = \case
      (b, "") -> Just b
      (_, _) -> Nothing
