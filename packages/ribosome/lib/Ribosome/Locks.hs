module Ribosome.Locks where

import qualified Polysemy.Conc.Sync as Sync

lockOrSkip ::
  Members [Sync lock, Resource] r =>
  Sem r a ->
  Sem r (Maybe a)
lockOrSkip ma =
  Sync.takeTry >>= traverse (finally ma . Sync.putTry)

lockOrSkip_ ::
  Members [Sync lock, Resource] r =>
  Sem r a ->
  Sem r ()
lockOrSkip_ =
  void . lockOrSkip
