module Ribosome.Host.Effect.Responses where

data Responses k v :: Effect where
  Add :: Responses k v m k
  Wait ::  k -> Responses k v m v
  Respond :: k -> v -> Responses k v m ()

makeSem ''Responses
