-- |Combinators for @optparse-applicative@.
module Ribosome.CliParser where

import Exon (exon)
import Log (Severity, parseSeverity)
import Options.Applicative (ReadM, readerError)
import Options.Applicative.Types (readerAsk)
import Path (Abs, Dir, File, Path, SomeBase (Abs, Rel), parseSomeDir, parseSomeFile, (</>))

-- |Convert a path to absolute, using the first argument as base dir for relative paths.
somePath ::
  Path Abs Dir ->
  SomeBase t ->
  Path Abs t
somePath cwd = \case
  Abs p ->
    p
  Rel p ->
    cwd </> p

-- |A logging severity option for @optparse-applicative@.
severityOption :: ReadM Severity
severityOption = do
  raw <- readerAsk
  maybe (readerError [exon|invalid log level: #{raw}|]) pure (parseSeverity (toText raw))

-- |Parse a path from a string in 'ReadM'.
readPath ::
  String ->
  (String -> Either e (SomeBase t)) ->
  Path Abs Dir ->
  String ->
  ReadM (Path Abs t)
readPath pathType parse cwd raw =
  either (const (readerError [exon|not a valid #{pathType} path: #{raw}|])) (pure . somePath cwd) (parse raw)

-- |A path option for @optparse-applicative@.
pathOption ::
  String ->
  (String -> Either e (SomeBase t)) ->
  Path Abs Dir ->
  ReadM (Path Abs t)
pathOption pathType parse cwd = do
  raw <- readerAsk
  readPath pathType parse cwd raw

-- |A directory path option for @optparse-applicative@.
dirPathOption ::
  Path Abs Dir ->
  ReadM (Path Abs Dir)
dirPathOption =
  pathOption "directory" parseSomeDir

-- |A file path option for @optparse-applicative@.
filePathOption ::
  Path Abs Dir ->
  ReadM (Path Abs File)
filePathOption =
  pathOption "file" parseSomeFile
