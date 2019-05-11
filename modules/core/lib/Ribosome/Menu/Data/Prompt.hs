{-# LANGUAGE TemplateHaskell #-}

module Ribosome.Menu.Data.Prompt where

import Ribosome.Menu.Data.PromptState (PromptState)

data Prompt =
  Prompt {
     _cursor :: Int,
     _state :: PromptState,
     _text :: Text
  }
  deriving (Eq, Show)

deepLenses ''Prompt
