{-# LANGUAGE TemplateHaskell #-}

module Ribosome.Menu.Prompt.Data.PromptState where

data PromptState =
  Insert
  |
  Normal
  deriving (Eq, Show)

deepLenses ''PromptState
