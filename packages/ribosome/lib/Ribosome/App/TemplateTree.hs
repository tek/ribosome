module Ribosome.App.TemplateTree where

import qualified Data.Text.IO as Text
import Path (Abs, Dir, File, Path, parent, toFilePath, (</>))
import Path.IO (createDirIfMissing, doesFileExist)

import Ribosome.App.Data (Global (..))
import Ribosome.App.Data.TemplateTree (TemplateTree (TDir, TFile))
import Ribosome.App.Error (RainbowError, ioError)
import Ribosome.App.UserInput (pathChunk)

warnExists :: Path Abs File -> Bool -> Sem r ()
warnExists _ = \case
  True ->
    undefined
  False ->
    unit

writeTemplate ::
  Members [Stop RainbowError, Embed IO] r =>
  Global ->
  Path Abs File ->
  Text ->
  Sem r ()
writeTemplate Global {..} path content = do
  stopTryIOError dirError (createDirIfMissing True dir)
  exists <- embed (doesFileExist path)
  if exists && not force
  then warnExists path quiet
  else stopTryIOError writeError (Text.writeFile (toFilePath path) content)
  where
    writeError msg =
      ioError ["Failed to write ", pathChunk path] msg
    dirError =
      ioError ["Failed to create directory ", pathChunk dir]
    dir =
      parent path

writeTemplateTree ::
  Members [Stop RainbowError, Embed IO] r =>
  Global ->
  Path Abs Dir ->
  TemplateTree ->
  Sem r ()
writeTemplateTree global current = \case
  TDir sub nodes ->
    traverse_ (writeTemplateTree global (current </> sub)) nodes
  TFile name content ->
    writeTemplate global (current </> name) content
