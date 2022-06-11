module Ribosome.Effect.Persist where

import Path (File, Path, Rel)

data Persist a :: Effect where
  Store :: Maybe (Path Rel File) -> a -> Persist a m ()
  Load :: Maybe (Path Rel File) -> Persist a m (Maybe a)

makeSem ''Persist

loadOr ::
  Member (Persist a) r =>
  Maybe (Path Rel File) ->
  a ->
  Sem r a
loadOr path a =
  fromMaybe a <$> load path

loadSingle ::
  Member (Persist a) r =>
  Sem r (Maybe a)
loadSingle =
  load Nothing

loadSingleOr ::
  Member (Persist a) r =>
  a ->
  Sem r a
loadSingleOr a =
  fromMaybe a <$> loadSingle

loadPath ::
  Member (Persist a) r =>
  Path Rel File ->
  Sem r (Maybe a)
loadPath path =
  load (Just path)

loadPathOr ::
  Member (Persist a) r =>
  Path Rel File ->
  a ->
  Sem r a
loadPathOr path a =
  fromMaybe a <$> loadPath path
