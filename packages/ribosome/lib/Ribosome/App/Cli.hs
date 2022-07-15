module Ribosome.App.Cli where

import Polysemy.Chronos (ChronosTime, interpretTimeChronos)
import System.Exit (exitFailure)
import System.IO (stderr)

import Ribosome.App.Boot (generateBoot)
import Ribosome.App.Data (Global (Global))
import Ribosome.App.Error (RainbowError, runRainbowErrorAnd)
import Ribosome.App.NewOptions (newOptions)
import Ribosome.App.NewProject (newProject)
import Ribosome.App.Options (Command (Boot, New), GlobalOptions (GlobalOptions), Options (..), parseCli)
import Ribosome.App.ProjectOptions (projectOptions)

runCommand ::
  Members [ChronosTime, Stop RainbowError, Embed IO] r =>
  Global ->
  Command ->
  Sem r ()
runCommand global = \case
  New opts -> do
    conf <- newOptions opts
    newProject global conf
  Boot opts -> do
    conf <- projectOptions opts
    generateBoot global conf

runOptions ::
  Members [ChronosTime, Stop RainbowError, Embed IO] r =>
  Options ->
  Sem r ()
runOptions (Options (GlobalOptions quiet force) cmd) =
  runCommand (Global (fromMaybe False quiet) (fromMaybe False force)) cmd

main :: IO ()
main = do
  conf <- parseCli
  runConc $ interpretTimeChronos (runRainbowErrorAnd stderr (embed exitFailure) (runOptions conf))
