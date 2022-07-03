module Ribosome.Lens where

import Control.Lens (ASetter, over)

(<|>~) :: Alternative f => ASetter s t (f a) (f a) -> f a -> s -> t
l <|>~ fa =
  over l (<|> fa)
{-# inline (<|>~) #-}
