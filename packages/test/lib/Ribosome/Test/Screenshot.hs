module Ribosome.Test.Screenshot where

import qualified Chiasma.Test.Screenshot as Chiasma
import Chiasma.TmuxApi (Tmux)
import Control.Lens ((.~))
import Control.Lens.Regex.Text (regex, group)
import Exon (exon)
import Hedgehog.Internal.Property (Failure)
import qualified Log
import Path (reldir)
import Polysemy.Chronos (ChronosTime)
import qualified Polysemy.Test as Test
import Polysemy.Test (Hedgehog, Test, TestError (TestError), (===))
import Prelude hiding (group)
import qualified Time
import Time (Seconds (Seconds))

import Ribosome.Test.Wait (assertWait)

-- |Nvim appears to add random whitespace sequences, optionally interspersed with color codes, to empty lines.
-- This remotes that noise from lines starting with `\ESC[94m~\ESC[39m`.
sanitize :: Text -> Text
sanitize =
  [regex|\x{1b}\[94m~\x{1b}\[39m(\s*(\x{1b}\[94m\s*\x{1b}\[39m\s*)?)$|] . group 0 .~ ""

screenshot ::
  Members [Tmux, Test, Error TestError, Embed IO] r =>
  Bool ->
  Text ->
  Int ->
  Sem r (Maybe ([Text], [Text]))
screenshot record name pane = do
  storage <- Test.fixturePath [reldir|screenshots|]
  mapError TestError (Chiasma.screenshotSanitized sanitize record storage name pane)

assertScreenshot ::
  Members [Tmux, Hedgehog IO, Test, Error TestError, Error Failure, ChronosTime, Race, Embed IO] r =>
  Text ->
  Int ->
  Sem r ()
assertScreenshot name pane = do
  assertWait (screenshot False name pane) (traverse_ check)
  where
    check (current, existing) =
      existing === current

updateScreeshot ::
  Members [Tmux, Hedgehog IO, Test, Error TestError, Error Failure, ChronosTime, Log, Race, Embed IO] r =>
  Text ->
  Int ->
  Sem r ()
updateScreeshot name pane = do
  Log.info [exon|Waiting for one second before storing new screenshot for '#{name}'|]
  Time.sleep (Seconds 1)
  void (screenshot True name pane)

awaitScreenshot ::
  Members [Tmux, Hedgehog IO, Test, Error TestError, Error Failure, ChronosTime, Log, Race, Embed IO] r =>
  Bool ->
  Text ->
  Int ->
  Sem r ()
awaitScreenshot = \case
  True ->
    updateScreeshot
  False ->
    assertScreenshot
