module Ribosome.App.Templates.MainHs where

import Exon (exon)

import Ribosome.App.Data (ModuleName (ModuleName))

mainHs :: ModuleName -> Text
mainHs (ModuleName modName) =
  [exon|module Main (main) where

import #{modName}.Plugin (main)
|]
