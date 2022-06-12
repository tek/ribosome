module Ribosome.Api.Path where

import Exon (exon)
import Path (Abs, Dir, File, Path, SomeBase (Abs, Rel), parseSomeDir, parseSomeFile, (</>))

import Ribosome.Host.Api.Effect (nvimCommand, vimCallFunction)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Path (pathText)

nvimCwd ::
  Member Rpc r =>
  Sem r (Path Abs Dir)
nvimCwd =
  vimCallFunction "getcwd" []

nvimSetCwd ::
  Member Rpc r =>
  Path Abs Dir ->
  Sem r ()
nvimSetCwd dir =
  nvimCommand [exon|cd #{pathText dir}|]

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

parseNvimDir ::
  Member Rpc r =>
  Text ->
  Sem r (Maybe (Path Abs Dir))
parseNvimDir =
  traverse nvimRelativePath . parseSomeDir . toString

parseNvimFile ::
  Member Rpc r =>
  Text ->
  Sem r (Maybe (Path Abs File))
parseNvimFile =
  traverse nvimRelativePath . parseSomeFile . toString
