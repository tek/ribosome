module Ribosome.App.NewProject where

import qualified Data.Text.IO as Text
import Polysemy.Chronos (ChronosTime)

import Ribosome.App.Boot (generateBoot)
import Ribosome.App.Data (Global (..), NewProject (..), Project (..), unPrintDir)
import qualified Ribosome.App.Data
import Ribosome.App.Error (RainbowError)
import Ribosome.App.TemplateTree (writeTemplateTree)
import Ribosome.App.Templates (newProjectTemplates)
import Ribosome.App.UserInput (cmdColor, infoMessage, neovimChunk, pathChunk, pathColor, putStderr)
import Ribosome.Host.Path (pathText)
import Exon (exon)

newProject ::
  Members [ChronosTime, Stop RainbowError, Embed IO] r =>
  Global ->
  NewProject ->
  Sem r ()
newProject global NewProject {project = pro@Project {..}, ..} = do
  writeTemplateTree global directory (newProjectTemplates names flakeUrl author maintainer branch github cachix)
  unless (global ^. #quiet) do
    infoMessage [
      "🌝 Initialized a ",
      neovimChunk,
      " plugin project in ",
      pathChunk directory,
      "."
      ]
    infoMessage [
      "Run ",
      cmdColor [exon|nix build .#{"#"}#{fromText names.name.unProjectName}.static|],
      " in that directory to create a statically linked executable in ",
      pathColor "result/bin",
      "."
      ]
    putStderr ""
  generateBoot global pro
  when printDir.unPrintDir (embed (Text.putStrLn (pathText directory)))
