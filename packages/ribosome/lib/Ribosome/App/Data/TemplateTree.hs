module Ribosome.App.Data.TemplateTree where

import Path (Dir, File, Path, Rel)

data TemplateTree =
  TDir (Path Rel Dir) [TemplateTree]
  |
  TFile (Path Rel File) Text
  deriving stock (Eq, Show)
