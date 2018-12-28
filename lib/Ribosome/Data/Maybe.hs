module Ribosome.Data.Maybe(
  orElse,
) where

orElse :: Maybe a -> Maybe a -> Maybe a
orElse fallback Nothing = fallback
orElse _ a = a
