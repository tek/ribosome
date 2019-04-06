module Ribosome.Test.Functional where

-- import Control.Exception (finally)
-- import Control.Monad (when)
-- import Control.Monad.IO.Class
-- import Data.Foldable (traverse_)
-- import Neovim (Neovim, vim_command')
-- import System.Console.ANSI (Color(Green), ColorIntensity(Dull), ConsoleLayer(Foreground), SGR(SetColor, Reset), setSGR)
-- import System.Directory (createDirectoryIfMissing, doesFileExist, getCurrentDirectory, makeAbsolute, removePathForcibly)
-- import System.FilePath (takeDirectory, takeFileName, (</>))

-- import Ribosome.Control.Ribo (Ribo)
-- import Ribosome.Test.Embed (TestConfig(..), setupPluginEnv, unsafeEmbeddedSpec)
-- import Ribosome.Test.Exists (waitForPlugin)
-- import qualified Ribosome.Test.File as F (fixture, tempDir)

-- jobstart :: MonadIO f => String -> f String
-- jobstart cmd = do
--   dir <- liftIO getCurrentDirectory
--   return $ "call jobstart('" ++ cmd ++ "', { 'rpc': v:true, 'cwd': '" ++ dir ++ "' })"

-- logFile :: TestConfig -> IO FilePath
-- logFile TestConfig{..} = makeAbsolute $ tcLogPath ++ "-spec"

-- startPlugin :: TestConfig -> Neovim env ()
-- startPlugin tc@TestConfig{..} = do
--   absLogPath <- liftIO $ makeAbsolute tcLogPath
--   absLogFile <- liftIO $ logFile tc
--   liftIO $ createDirectoryIfMissing True (takeDirectory absLogPath)
--   liftIO $ removePathForcibly absLogFile
--   setupPluginEnv tc
--   cmd <- jobstart $ "stack run -- -l " ++ absLogFile ++ " -v INFO"
--   vim_command' cmd
--   waitForPlugin tcPluginName 0.1 3

-- fSpec :: TestConfig -> Neovim env () -> Neovim env ()
-- fSpec conf spec = startPlugin conf >> spec

-- showLog' :: String -> IO ()
-- showLog' output = do
--   putStrLn ""
--   setSGR [SetColor Foreground Dull Green]
--   putStrLn "plugin output:"
--   setSGR [Reset]
--   traverse_ putStrLn (lines output)
--   putStrLn ""

-- showLog :: TestConfig -> IO ()
-- showLog conf = do
--   file <- logFile conf
--   exists <- doesFileExist file
--   when exists $ do
--     output <- readFile file
--     case output of
--       [] -> return ()
--       o -> showLog' o

-- functionalSpec :: TestConfig -> Ribo () () -> IO ()
-- functionalSpec conf spec =
--   finally (unsafeEmbeddedSpec fSpec conf () spec) (showLog conf)

-- fPrefix :: String
-- fPrefix = "f"

-- tempDir :: FilePath -> Neovim e FilePath
-- tempDir = F.tempDir fPrefix

-- tempFile :: FilePath -> Neovim e FilePath
-- tempFile file = do
--   absDir <- tempDir $ takeDirectory file
--   return $ absDir </> takeFileName file

-- fixture :: MonadIO m => FilePath -> m FilePath
-- fixture = F.fixture fPrefix
