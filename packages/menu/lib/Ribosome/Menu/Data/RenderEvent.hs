module Ribosome.Menu.Data.RenderEvent where

newtype RenderEvent =
  RenderEvent { unRenderEvent :: Text }
  deriving stock (Eq, Show)
  deriving newtype (IsString)
