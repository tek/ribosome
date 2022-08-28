-- |An effect used internally to execute handlers when Neovim variables are changed.
module Ribosome.Effect.VariableWatcher where

-- |The name of a variable that Ribosome should watch for changes.
newtype WatchedVariable =
  WatchedVariable { unWatchedVariable :: Text }
  deriving stock (Eq, Show)
  deriving newtype (IsString, Ord)

-- |An effect used internally to execute handlers when Neovim variables are changed.
data VariableWatcher :: Effect where
  -- |Called when the internal logic determines that variables should be examined for updates.
  Update :: VariableWatcher m ()
  -- |Stop running update handlers for the given variable.
  Unwatch :: WatchedVariable -> VariableWatcher m ()

makeSem_ ''VariableWatcher

-- |Called when the internal logic determines that variables should be examined for updates.
update ::
  Member VariableWatcher r =>
  Sem r ()

-- |Stop running update handlers for the given variable.
unwatch ::
  Member VariableWatcher r =>
  WatchedVariable ->
  Sem r ()
