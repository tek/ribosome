module Ribosome.Test.Functional(
  startPlugin,
  fSpec,
  functionalSpec,
  tempDir,
  tempFile,
  fixture,
) where

import Control.Monad (when)
import Control.Monad.IO.Class
import Control.Exception (finally)
import Data.Foldable (traverse_)
import System.Directory (getCurrentDirectory, createDirectoryIfMissing, removePathForcibly, doesFileExist, makeAbsolute)
import System.FilePath (takeDirectory, (</>), takeFileName)
import System.Console.ANSI (setSGR, SGR(SetColor, Reset), ConsoleLayer(Foreground), ColorIntensity(Dull), Color(Green))
import Neovim (Neovim, vim_command')
import Ribosome.Control.Ribo (Ribo)
import Ribosome.Test.Exists (waitForPlugin)
import Ribosome.Test.Embed (TestConfig(..), unsafeEmbeddedSpec, setupPluginEnv)
import qualified Ribosome.Test.File as F (tempDir, fixture)

jobstart :: MonadIO f => String -> f String
jobstart cmd = do
  dir <- liftIO getCurrentDirectory
  return $ "call jobstart('" ++ cmd ++ "', { 'rpc': v:true, 'cwd': '" ++ dir ++ "' })"

logFile :: TestConfig -> IO FilePath
logFile conf = makeAbsolute $ logPath conf ++ "-spec"

startPlugin :: TestConfig -> Neovim env ()
startPlugin conf = do
  absLogPath <- liftIO $ makeAbsolute (logPath conf)
  absLogFile <- liftIO $ logFile conf
  liftIO $ createDirectoryIfMissing True (takeDirectory absLogPath)
  liftIO $ removePathForcibly absLogFile
  setupPluginEnv conf
  cmd <- jobstart $ "stack run -- -l " ++ absLogFile ++ " -v INFO"
  vim_command' cmd
  waitForPlugin (pluginName conf) 0.1 3

fSpec :: TestConfig -> Neovim env () -> Neovim env ()
fSpec conf spec = startPlugin conf >> spec

showLog' :: String -> IO ()
showLog' output = do
  putStrLn ""
  setSGR [SetColor Foreground Dull Green]
  putStrLn "plugin output:"
  setSGR [Reset]
  traverse_ putStrLn (lines output)
  putStrLn ""

showLog :: TestConfig -> IO ()
showLog conf = do
  file <- logFile conf
  exists <- doesFileExist file
  when exists $ do
    output <- readFile file
    case output of
      [] -> return ()
      o -> showLog' o

functionalSpec :: TestConfig -> Ribo () () -> IO ()
functionalSpec conf spec =
  finally (unsafeEmbeddedSpec fSpec conf () spec) (showLog conf)

fPrefix :: String
fPrefix = "f"

tempDir :: FilePath -> Neovim e FilePath
tempDir = F.tempDir fPrefix

tempFile :: FilePath -> Neovim e FilePath
tempFile file = do
  absDir <- tempDir $ takeDirectory file
  return $ absDir </> takeFileName file

fixture :: MonadIO m => FilePath -> m FilePath
fixture = F.fixture fPrefix
