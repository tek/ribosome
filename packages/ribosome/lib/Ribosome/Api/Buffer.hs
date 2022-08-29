-- |API functions for buffers.
module Ribosome.Api.Buffer where

import qualified Data.Text as Text (null)
import Exon (exon)
import Path (Abs, Dir, File, Path, parseAbsFile, parseRelFile, (</>))

import Ribosome.Api.Path (nvimCwd)
import Ribosome.Data.FileBuffer (FileBuffer (FileBuffer))
import qualified Ribosome.Host.Api.Data as Data
import Ribosome.Host.Api.Data (Buffer)
import Ribosome.Host.Api.Effect (
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
import Ribosome.Host.Class.Msgpack.Encode (toMsgpack)
import Ribosome.Host.Data.RpcError (RpcError)
import qualified Ribosome.Host.Effect.Rpc as Rpc
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
  Member Rpc r =>
  Buffer ->
  Sem r [Text]
bufferContent buffer =
  bufferGetLines buffer 0 (-1) False

-- |Return the entire content of the current buffer.
currentBufferContent ::
  Member Rpc r =>
  Sem r [Text]
currentBufferContent =
  bufferContent =<< vimGetCurrentBuffer

-- |Replace the content of the given buffer.
setBufferContent ::
  Member Rpc r =>
  Buffer ->
  [Text] ->
  Sem r ()
setBufferContent buffer =
  bufferSetLines buffer 0 (-1) False

-- |Replace the content of the current buffer.
setCurrentBufferContent ::
  Member Rpc r =>
  [Text] ->
  Sem r ()
setCurrentBufferContent content = do
  buffer <- vimGetCurrentBuffer
  setBufferContent buffer content

-- |Replace a single line in the given buffer.
setBufferLine ::
  Member Rpc r =>
  Buffer ->
  Int ->
  Text ->
  Sem r ()
setBufferLine buffer line text =
  bufferSetLines buffer line (line + 1) False [text]

-- |Execute an action only if the given buffer is valid, i.e. it exists but may be unloaded.
whenValid ::
  Member Rpc r =>
  (Buffer -> Sem r ()) ->
  Buffer ->
  Sem r ()
whenValid use buffer =
  whenM (bufferIsValid buffer) (use buffer)

-- |Execute an action with the given buffer's number if it is valid.
withBufferNumber ::
  Member Rpc r =>
  (Int -> Sem r ()) ->
  Buffer ->
  Sem r ()
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
  Member Rpc r =>
  Buffer ->
  Sem r ()
wipeBuffer =
  whenValid \ b -> nvimBufDelete b [("force", toMsgpack True)]

-- |Force-unload a buffer, discarding changes.
unloadBuffer ::
  Member Rpc r =>
  Buffer ->
  Sem r ()
unloadBuffer =
  whenValid \ b -> nvimBufDelete b [("force", toMsgpack True), ("unload", toMsgpack True)]

-- |Add a buffer to the list without loading it.
addBuffer ::
  Member Rpc r =>
  Text ->
  Sem r ()
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
  Member Rpc r =>
  Sem r [FileBuffer]
fileBuffers = do
  cwd <- nvimCwd
  buffers <- vimGetBuffers
  names <- Rpc.sync (foldMap (fmap pure . Data.bufferGetName) buffers)
  pure (catMaybes (zipWith (fileBuffer cwd) buffers names))

-- |Find the buffer whose name is the given path.
bufferForFile ::
  Member Rpc r =>
  Path Abs File ->
  Sem r (Maybe FileBuffer)
bufferForFile target =
  find sameBuffer <$> fileBuffers
  where
    sameBuffer (FileBuffer _ path) =
      path == target

-- |Return the name of the current buffer.
currentBufferName ::
  Member Rpc r =>
  Sem r Text
currentBufferName =
  bufferGetName =<< vimGetCurrentBuffer

-- |Set the current buffer.
setCurrentBuffer ::
  Member Rpc r =>
  Buffer ->
  Sem r ()
setCurrentBuffer buf = do
  win <- vimGetCurrentWindow
  nvimWinSetBuf win buf

-- |Indicate whether the given buffer is a file, i.e. has empty @buftype@.
bufferIsFile ::
  Member Rpc r =>
  Buffer ->
  Sem r Bool
bufferIsFile buf =
  Text.null <$> bufferGetOption buf "buftype"

-- |Return the number of buffers in the list.
bufferCount ::
  Member Rpc r =>
  Sem r Natural
bufferCount =
  fromIntegral . length <$> vimGetBuffers

-- |Return the file system path of the given buffer, if its name is a valid path.
bufferPath ::
  Member Rpc r =>
  Buffer ->
  Sem r (Maybe (Path Abs File))
bufferPath buffer = do
  Rpc.sync do
    cwd <- Data.vimCallFunction "getcwd" []
    name <- Data.bufferGetName buffer
    pure (fileBuffer cwd buffer name <&> \ (FileBuffer _ path) -> path)

-- |Return the file system path of the current buffer, if its name is a valid path.
currentBufferPath ::
  Member Rpc r =>
  Sem r (Maybe (Path Abs File))
currentBufferPath =
  bufferPath =<< nvimGetCurrentBuf

-- |Filter out the unlisted buffers from the given list.
filterListed ::
  Member Rpc r =>
  [Buffer] ->
  Sem r [Buffer]
filterListed bufs = do
  nums <- Rpc.sync (traverse Data.bufferGetNumber bufs)
  listedFlags <- Rpc.sync (traverse (Data.nvimCallFunction "buflisted" . pure . toMsgpack) nums)
  let listed = mapMaybe chooseListed (zip bufs listedFlags)
  buftypes :: [Text] <- Rpc.sync (traverse (\ buf -> Data.bufferGetOption buf "buftype") listed)
  pure (mapMaybe chooseEmptyTypes (zip bufs buftypes))
  where
    chooseListed (b, l) =
      if l then Just b else Nothing
    chooseEmptyTypes = \case
      (b, "") -> Just b
      (_, _) -> Nothing
