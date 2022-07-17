module Ribosome.Test.NewProjectTest where

import qualified Chronos
import Chronos (datetimeToTime)
import qualified Data.Text as Text
import Path (reldir, relfile)
import Polysemy.Chronos (interpretTimeChronosConstant)
import qualified Polysemy.Test as Test
import Polysemy.Test (UnitTest, runTestAuto, (===))
import System.IO (stderr)
import Time (mkDatetime)

import Ribosome.App.Data (Cachix (Cachix), Github (Github), NewProject (..), PrintDir (PrintDir), Project (..))
import Ribosome.App.Error (runRainbowErrorAnd)
import Ribosome.App.NewOptions (defaultFlakeUrl)
import Ribosome.App.NewProject (newProject)
import qualified Ribosome.App.ProjectNames as ProjectNames

testTime :: Chronos.Time
testTime =
  datetimeToTime (mkDatetime 2300 1 1 12 0 0)

test_newProject :: UnitTest
test_newProject =
  runTestAuto $ interpretTimeChronosConstant testTime do
    dir <- Test.tempDir [reldir|new-project|]
    names <- fromEither (ProjectNames.parse "test-project")
    runRainbowErrorAnd stderr (fail "project generation failed") do
      newProject def NewProject {
        project = Project {
          names,
          directory = dir,
          branch = "main",
          github = Just (Github "org" "rep"),
          cachix = Just (Cachix "cach" "12345")
        },
        flakeUrl = defaultFlakeUrl,
        printDir = PrintDir False,
        author = "author",
        maintainer = "maintainer@home.page"
      }
    targetAction <- Test.fixtureLines [relfile|new-project/action.yml|]
    generatedAction <- Test.tempFileContent [relfile|new-project/.github/workflows/latest.yml|]
    targetAction === (Text.lines generatedAction)
