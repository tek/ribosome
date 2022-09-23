-- |API function for file system paths.
module Ribosome.Api.Path where

import Exon (exon)
import Path (Abs, Dir, File, Path, SomeBase (Abs, Rel), parseSomeDir, parseSomeFile, (</>))

import qualified Ribosome.Host.Api.Data as Data
import Ribosome.Host.Api.Data (nvimCommand)
import Ribosome.Host.Class.MonadRpc (MonadRpc)
import Ribosome.Host.Data.Report (Report)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Host.Path (pathText)
import Ribosome.Internal.Path (failInvalidPath)

-- |Get Neovim's current working directory.
nvimCwd ::
  MonadRpc m =>
  m (Path Abs Dir)
nvimCwd =
  Data.vimCallFunction "getcwd" []

-- |Set Neovim's current working directory.
nvimSetCwd ::
  MonadRpc m =>
  Path Abs Dir ->
  m ()
nvimSetCwd dir =
  nvimCommand [exon|cd #{pathText dir}|]

-- |Convert an abstract path to an absolute one, using the supplied directory as the base for relative paths.
relativePathAt ::
  Path Abs Dir ->
  SomeBase t ->
  Path Abs t
relativePathAt cwd = \case
  Abs p ->
    p
  Rel p ->
    cwd </> p

-- |Convert an abstract path to an absolute one, using Neovim's current working directory as the base for relative
-- paths.
nvimRelativePath ::
  Member Rpc r =>
  SomeBase t ->
  Sem r (Path Abs t)
nvimRelativePath = \case
  Abs p ->
    pure p
  Rel p -> do
    cwd <- nvimCwd
    pure (cwd </> p)

-- |Parse a directory path and prepend Neovim's current working directory to it if it's relative.
parseNvimDir ::
  Member Rpc r =>
  Text ->
  Sem r (Maybe (Path Abs Dir))
parseNvimDir "" =
  Just <$> nvimCwd
parseNvimDir p =
  traverse nvimRelativePath (parseSomeDir (toString p))

-- |Parse a directory path and prepend the supplied directory to it if it's relative.
parseDirAt ::
  Path Abs Dir ->
  Text ->
  Maybe (Path Abs Dir)
parseDirAt cwd "" =
  pure cwd
parseDirAt cwd p =
  relativePathAt cwd <$> parseSomeDir (toString p)

-- |Parse a list of directory paths and prepend Neovim's current working directory to it if a path is relative.
parseNvimDirs ::
  Member Rpc r =>
  [Text] ->
  Sem r [Path Abs Dir]
parseNvimDirs paths = do
  cwd <- nvimCwd
  pure (mapMaybe (parseDirAt cwd) paths)

-- |Parse a file path and prepend Neovim's current working directory to it if it's relative.
parseNvimFile ::
  Member Rpc r =>
  Text ->
  Sem r (Maybe (Path Abs File))
parseNvimFile =
  traverse nvimRelativePath . parseSomeFile . toString

-- |Parse a file path and prepend the supplied directory to it if it's relative.
parseFileAt ::
  Path Abs Dir ->
  Text ->
  Maybe (Path Abs File)
parseFileAt cwd p =
  relativePathAt cwd <$> parseSomeFile (toString p)

-- |Parse a list of file paths and prepend Neovim's current working directory to it if a path is relative.
parseNvimFiles ::
  Member Rpc r =>
  [Text] ->
  Sem r [Path Abs File]
parseNvimFiles paths = do
  cwd <- nvimCwd
  pure (mapMaybe (parseFileAt cwd) paths)

-- |Parse a directory path and prepend Neovim's current working directory to it if it's relative.
--
-- If parsing fails, emit an error 'Report'.
nvimDir ::
  Members [Rpc, Stop Report] r =>
  Text ->
  Sem r (Path Abs Dir)
nvimDir spec =
  failInvalidPath spec =<< parseNvimDir spec

-- |Parse a file path and prepend Neovim's current working directory to it if it's relative.
--
-- If parsing fails, emit an error 'Report'.
nvimFile ::
  Members [Rpc, Stop Report] r =>
  Text ->
  Sem r (Path Abs File)
nvimFile spec =
  failInvalidPath spec =<< parseNvimFile spec
