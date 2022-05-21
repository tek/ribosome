module Ribosome.Effect.Persist where

data Persist a :: Effect where
  Store :: a -> Persist a m ()
  Load :: Persist a m (Maybe a)

makeSem ''Persist

loadOr ::
  Member (Persist a) r =>
  a ->
  Sem r a
loadOr a =
  fromMaybe a <$> load
