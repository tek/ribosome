module Ribosome.Host.Interpret where

type (|>) :: [k] -> k -> [k]
type (a :: [k]) |> (b :: k) =
  a ++ '[b]

infixl 6 |>

type HigherOrder r r' =
  Members r' (r ++ r')

with :: Sem r a -> (a -> InterpreterFor eff r) -> InterpreterFor eff r
with acquire f sem = do
  a <- acquire
  f a sem
{-# inline with #-}
