module Ribosome.Data.Functor where

infixr 1 <$<

(<$<) :: Functor f => (b -> c) -> (a -> f b) -> (a -> f c)
(<$<) f =
  (.) (fmap f)
