module Ribosome.Host.Interpret where

with :: Sem r a -> (a -> InterpreterFor eff r) -> InterpreterFor eff r
with acquire f sem = do
  a <- acquire
  f a sem
{-# inline with #-}
