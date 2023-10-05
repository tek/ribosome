-- | Functions for simulating user input in tests.
module Ribosome.Api.Input where

import qualified Polysemy.Time as Time
import Time (NanoSeconds)

import Ribosome.Host.Api.Data (nvimInput, nvimReplaceTermcodes, nvimFeedkeys)
import Ribosome.Host.Effect.Rpc (Rpc)

-- | Send a list of character sequences as user input to Neovim with an optional wait interval.
--
-- Uses @nvim_input@.
syntheticInput ::
  Members [Rpc, Time t d] r =>
  Maybe NanoSeconds ->
  [Text] ->
  Sem r ()
syntheticInput interval =
  traverse_ \ c ->
    traverse_ Time.sleep interval *> nvimInput c

-- | Options for the function @nvim_replace_termcodes@, used by 'feedKeyWith'.
data ReplaceTermcodesOptions =
  -- | Replace all nvim keycodes like @<cr>@ with their byte representation.
  -- The default.
  ReplaceKeycodes
  |
  -- | Like 'ReplaceKeycodes', but also replace @<lt>@, which is usually used to escape @<@.
  ReplaceKeycodesAndLt
  |
  -- | Don't replace keycodes, only terminal codes.
  ReplaceNoKeycodes
  deriving stock (Eq, Show, Generic)

-- | Options for 'feedKeyWith'.
data FeedKeyOptions =
  FeedKeyOptions {
    -- | Whether to convert keycodes like @<cr>@ to bytes.
    -- If this is disabled, they will be interpreted as multiple typed characters instead of their nvim meaning.
    -- Default is to replace keycodes except for @<lt>@.
    replaceTermcodes :: ReplaceTermcodesOptions,

    -- | Whether to trigger mappings matching the typed keys.
    -- Default on.
    remap :: Bool,

    -- | Behave as if the keys are typed, rather than from a mapping.
    -- When off, everything will be bundled in a single undo entry etc.
    -- Default on.
    typed :: Bool,

    -- | If the keys are fed by a mapping while the typeahead buffer is nonempty, this flag can force execution of the
    -- fed keys before the rest of typeahead is executed.
    -- Default off.
    insert :: Bool,

    -- | Execute commands immediately, without waiting for more input when an imcomplete command is fed.
    -- Default off.
    execute :: Bool,

    -- | When 'execute' is on, don't leave insert mode after execution.
    -- Default off.
    executeKeepInsert :: Bool
  }
  deriving stock (Eq, Show, Generic)

instance Default FeedKeyOptions where
  def =
    FeedKeyOptions {
      replaceTermcodes = ReplaceKeycodes,
      remap = True,
      typed = True,
      insert = False,
      execute = False,
      executeKeepInsert = False
    }

-- | Send a sequence of keys using @nvim_feedkeys@ after replacing terminal codes.
feedKeyWith ::
  Member Rpc r =>
  FeedKeyOptions ->
  Text ->
  Sem r ()
feedKeyWith FeedKeyOptions {..} k = do
  key <- nvimReplaceTermcodes k True doLt special
  nvimFeedkeys key (mconcat mode) False
  where
    (doLt, special) = case replaceTermcodes of
      ReplaceKeycodes -> (False, True)
      ReplaceKeycodesAndLt -> (True, True)
      ReplaceNoKeycodes -> (False, False)

    mode =
      [
        modeBit "m" "n" remap,
        modeBit "t" "" typed,
        modeBit "i" "" insert,
        modeBit "x" "" execute,
        modeBit "!" "" executeKeepInsert
      ]

    modeBit yes no = \case
      True -> yes
      False -> no

-- | Send a sequence of keys using @nvim_feedkeys@ after replacing terminal codes.
feedKey ::
  Member Rpc r =>
  Text ->
  Sem r ()
feedKey =
  feedKeyWith def

-- | Send a list of character sequences as user input to Neovim with an optional wait interval.
--
-- Uses @nvim_feedkeys@.
feedKeys ::
  Members [Rpc, Time t d] r =>
  Maybe NanoSeconds ->
  [Text] ->
  Sem r ()
feedKeys interval =
  traverse_ \ c ->
    traverse_ Time.sleep interval *> feedKey c
