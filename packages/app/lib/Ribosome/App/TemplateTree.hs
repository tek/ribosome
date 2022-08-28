module Ribosome.App.TemplateTree where

import qualified Data.Text.IO as Text
import Path (Abs, Dir, File, Path, Rel, parent, reldir, toFilePath, (</>))
import Path.IO (createDirIfMissing, doesFileExist)

import Ribosome.App.Data (Global (..))
import Ribosome.App.Data.TemplateTree (TemplateTree (TDir, TFile))
import Ribosome.App.Error (RainbowError, ioError)
import Ribosome.App.UserInput (cmdColor, infoMessage, pathChunk)

warnExists ::
  Members [Stop RainbowError, Embed IO] r =>
  Path Rel File ->
  Sem r ()
warnExists file =
  infoMessage [
    "⚠️ Not overwriting ",
    pathChunk file,
    " without ",
    cmdColor "--force"
  ]

writeTemplate ::
  Members [Stop RainbowError, Embed IO] r =>
  Global ->
  Path Abs File ->
  Path Rel File ->
  Text ->
  Sem r ()
writeTemplate Global {..} path relPath content = do
  stopTryIOError dirError (createDirIfMissing True dir)
  exists <- embed (doesFileExist path)
  if exists && not force
  then unless quiet (warnExists relPath)
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
writeTemplateTree global root =
  spin [reldir|.|]
  where
    spin current = \case
      TDir sub nodes ->
        traverse_ (spin (current </> sub)) nodes
      TFile name content ->
        let file = current </> name
        in writeTemplate global (root </> file) file content
