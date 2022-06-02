module Ribosome.Final where

import Polysemy.Final (withWeavingToFinal)

inFinal ::
  ∀ r a .
  Member (Final IO) r =>
  (∀ f . Functor f => f () -> (∀ x . Sem r x -> IO (f x)) -> (∀ x . x -> IO (f x)) -> (∀ x . f x -> Maybe x) -> IO (f a)) ->
  Sem r a
inFinal f =
  withWeavingToFinal \ s wv ex ->
    f s (\ ma -> wv (ma <$ s)) (\ a -> pure (a <$ s)) ex

inFinal_ ::
  ∀ r a .
  Member (Final IO) r =>
  (∀ f . Functor f => (∀ x . Sem r x -> IO (Maybe x)) -> (Sem r () -> IO ()) -> (∀ x . x -> IO (f x)) -> IO (f a)) ->
  Sem r a
inFinal_ f =
  withWeavingToFinal \ s wv ex ->
    let
      lowerMaybe :: ∀ x . Sem r x -> IO (Maybe x)
      lowerMaybe sem =
        ex <$> wv (sem <$ s)
      lowerUnit :: ∀ x . Sem r x -> IO ()
      lowerUnit =
        fmap (fromMaybe ()) . lowerMaybe . void
    in f lowerMaybe lowerUnit (\ a -> pure (a <$ s))
