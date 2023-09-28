module Ribosome.Menu.Data.RenderEvent where

import Ribosome.Menu.Data.MenuAction (RenderAnchor)

data RenderEvent =
  RenderEvent {
    desc :: Text,
    anchor :: RenderAnchor
  }
  deriving stock (Eq, Show)
