-- |API function for file system paths.
module Ribosome.Api.Path where

import Exon (exon)
import Path (Abs, Dir, File, Path, SomeBase (Abs, Rel), parseSomeDir, parseSomeFile, (</>))

import Ribosome.Host.Api.Effect (nvimCommand, vimCallFunction)
import Ribosome.Host.Data.Report (Report)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Internal.Path (failInvalidPath)
import Ribosome.Path (pathText)

-- |Get Neovim's current working directory.
nvimCwd ::
  Member Rpc r =>
  Sem r (Path Abs Dir)
nvimCwd =
  vimCallFunction "getcwd" []

-- |Set Neovim's current working directory.
nvimSetCwd ::
  Member Rpc r =>
  Path Abs Dir ->
  Sem r ()
nvimSetCwd dir =
  nvimCommand [exon|cd #{pathText dir}|]

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

-- |Parse a file path and prepend Neovim's current working directory to it if it's relative.
parseNvimFile ::
  Member Rpc r =>
  Text ->
  Sem r (Maybe (Path Abs File))
parseNvimFile =
  traverse nvimRelativePath . parseSomeFile . toString

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
