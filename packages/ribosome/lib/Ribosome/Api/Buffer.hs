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
import Ribosome.Path (pathText)

edit ::
  Member Rpc r =>
  Path b t ->
  Sem r ()
edit path = nvimCommand [exon|silent! edit #{pathText path}|]

buflisted ::
  Member (Rpc !! RpcError) r =>
  Buffer ->
  Sem r Bool
buflisted buf = do
  resumeAs False do
    num <- bufferGetNumber buf
    nvimCallFunction "buflisted" [toMsgpack num]

bufferContent ::
  Member Rpc r =>
  Buffer ->
  Sem r [Text]
bufferContent buffer =
  bufferGetLines buffer 0 (-1) False

currentBufferContent ::
  Member Rpc r =>
  Sem r [Text]
currentBufferContent =
  bufferContent =<< vimGetCurrentBuffer

setBufferContent ::
  Member Rpc r =>
  Buffer ->
  [Text] ->
  Sem r ()
setBufferContent buffer =
  bufferSetLines buffer 0 (-1) False

setBufferLine :: Member Rpc r => Buffer -> Int -> Text -> Sem r ()
setBufferLine buffer line text =
  bufferSetLines buffer line (line + 1) False [text]

setCurrentBufferContent ::
  Member Rpc r =>
  [Text] ->
  Sem r ()
setCurrentBufferContent content = do
  buffer <- vimGetCurrentBuffer
  setBufferContent buffer content

whenValid ::
  Member Rpc r =>
  (Buffer -> Sem r ()) ->
  Buffer ->
  Sem r ()
whenValid use buffer =
  whenM (bufferIsValid buffer) (use buffer)

withBufferNumber ::
  Member Rpc r =>
  (Int -> Sem r ()) ->
  Buffer ->
  Sem r ()
withBufferNumber run =
 whenValid (run <=< bufferGetNumber)

closeBuffer ::
  Member Rpc r =>
  Buffer ->
  Sem r ()
closeBuffer =
  silentBang . withBufferNumber del
  where
    del number =
      nvimCommand [exon|bdelete! #{show number}|]

wipeBuffer ::
  Member Rpc r =>
  Buffer ->
  Sem r ()
wipeBuffer =
  whenValid \ b -> nvimBufDelete b [("force", toMsgpack True)]

unloadBuffer ::
  Member Rpc r =>
  Buffer ->
  Sem r ()
unloadBuffer =
  whenValid \ b -> nvimBufDelete b [("force", toMsgpack True), ("unload", toMsgpack True)]

addBuffer ::
  Member Rpc r =>
  Text ->
  Sem r ()
addBuffer path =
  nvimCommand [exon|badd #{path}|]

fileBuffer ::
  Path Abs Dir ->
  Buffer ->
  Text ->
  Maybe FileBuffer
fileBuffer cwd buffer (toString -> path) =
  FileBuffer buffer <$> (parseAbsFile path <|> (cwd </>) <$> parseRelFile path)

fileBuffers ::
  Member Rpc r =>
  Sem r [FileBuffer]
fileBuffers = do
  cwd <- nvimCwd
  buffers <- vimGetBuffers
  names <- Rpc.sync (foldMap (fmap pure . Data.bufferGetName) buffers)
  pure (catMaybes (zipWith (fileBuffer cwd) buffers names))

bufferForFile ::
  Member Rpc r =>
  Path Abs File ->
  Sem r (Maybe FileBuffer)
bufferForFile target =
  find sameBuffer <$> fileBuffers
  where
    sameBuffer (FileBuffer _ path) =
      path == target

currentBufferName ::
  Member Rpc r =>
  Sem r Text
currentBufferName =
  bufferGetName =<< vimGetCurrentBuffer

setCurrentBuffer ::
  Member Rpc r =>
  Buffer ->
  Sem r ()
setCurrentBuffer buf = do
  win <- vimGetCurrentWindow
  nvimWinSetBuf win buf

bufferIsFile ::
  Member Rpc r =>
  Buffer ->
  Sem r Bool
bufferIsFile buf =
  Text.null <$> bufferGetOption buf "buftype"

bufferCount ::
  Member Rpc r =>
  Sem r Natural
bufferCount =
  fromIntegral . length <$> vimGetBuffers

bufferPath ::
  Member Rpc r =>
  Buffer ->
  Sem r (Maybe (Path Abs File))
bufferPath buffer = do
  Rpc.sync do
    cwd <- Data.vimCallFunction "getcwd" []
    name <- Data.bufferGetName buffer
    pure (fileBuffer cwd buffer name <&> \ (FileBuffer _ path) -> path)

currentBufferPath ::
  Member Rpc r =>
  Sem r (Maybe (Path Abs File))
currentBufferPath =
  bufferPath =<< nvimGetCurrentBuf

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
