module Ribosome.App.NewProject where

import qualified Data.Text.IO as Text
import Polysemy.Chronos (ChronosTime)
import qualified Time

import Ribosome.App.Boot (generateBoot)
import Ribosome.App.Data (Global (..), NewProject (..), Project (..), unPrintDir)
import Ribosome.App.Error (RainbowError)
import Ribosome.App.TemplateTree (writeTemplateTree)
import Ribosome.App.Templates (newProjectTemplates)
import Ribosome.App.UserInput (cmdColor, infoMessage, neovimChunk, pathChunk, pathColor, putStderr)
import Ribosome.Path (pathText)

newProject ::
  Members [ChronosTime, Stop RainbowError, Embed IO] r =>
  Global ->
  NewProject ->
  Sem r ()
newProject global NewProject {project = pro@Project {..}, ..} = do
  year <- fromIntegral . Time.year <$> Time.today
  writeTemplateTree global directory (newProjectTemplates names flakeUrl author maintainer year github)
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
      cmdColor "nix build .#static",
      " in that directory to create a statically linked executable in ",
      pathColor "result/bin",
      "."
      ]
    putStderr ""
  generateBoot global pro
  when (unPrintDir printDir) (embed (Text.putStrLn (pathText directory)))
