{-# options_haddock prune, hide #-}

-- |Internal combinators for paths.
module Ribosome.Internal.Path where

import Exon (exon)

import Ribosome.Host.Data.Report (Report)

failInvalidPath ::
  Member (Stop Report) r =>
  Text ->
  Maybe a ->
  Sem r a
failInvalidPath spec result =
  withFrozenCallStack do
    stopNote (fromText [exon|Invalid path: #{spec}|]) result
