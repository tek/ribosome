{-# options_haddock prune, hide #-}

-- |Lens combinators, used internally
module Ribosome.Lens where

(<|>~) :: Alternative f => ASetter s t (f a) (f a) -> f a -> s -> t
l <|>~ fa =
  over l (<|> fa)
{-# inline (<|>~) #-}
