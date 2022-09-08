module Ribosome.Test.Screenshot where

import Chiasma.Data.CodecError (CodecError)
import Chiasma.Effect.Codec (NativeCommandCodecE)
import Chiasma.Effect.TmuxClient (NativeTmux)
import qualified Chiasma.Test.Screenshot as Chiasma
import Chiasma.Tmux (withTmux)
import Chiasma.TmuxApi (Tmux)
import Control.Lens.Regex.Text (group, regex)
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

import Ribosome.Host.Effect.Log (StderrLog, stderrLog)
import Ribosome.Test.Wait (assertWait)

-- |Nvim appears to add random whitespace sequences, optionally interspersed with color codes, to empty lines.
-- This removes that noise from lines starting with @~\ESC[39m@ or @\ESC[94m~\ESC[39m@.
sanitize :: Text -> Text
sanitize =
  [regex|(\x{1b}\[94m)?~((\s|\x{1b}\[94m|\x{1b}\[39m)*)$|] . group 1 .~ ""

screenshot ::
  Members [Tmux, Test, Error TestError, Embed IO] r =>
  Bool ->
  Bool ->
  Text ->
  Int ->
  Sem r (Maybe ([Text], [Text]))
screenshot record sane name pane = do
  storage <- Test.fixturePath [reldir|screenshots|]
  mapError TestError (Chiasma.screenshotSanitized (if sane then sanitize else id) record storage name pane)

assertScreenshot ::
  HasCallStack =>
  Members [NativeTmux, NativeCommandCodecE, Stop CodecError] r =>
  Members [Hedgehog IO, Test, Error TestError, Error Failure, ChronosTime, Race, Embed IO] r =>
  Bool ->
  Text ->
  Int ->
  Sem r ()
assertScreenshot sane name pane =
  withFrozenCallStack $ withTmux $ restop do
    assertWait (screenshot False sane name pane) (traverse_ check)
  where
    check (current, existing) =
      existing === current

updateScreeshot ::
  HasCallStack =>
  Members [NativeTmux, NativeCommandCodecE, Stop CodecError] r =>
  Members [Hedgehog IO, Test, Error TestError, Error Failure, ChronosTime, StderrLog, Race, Embed IO] r =>
  Bool ->
  Text ->
  Int ->
  Sem r ()
updateScreeshot sane name pane =
  withTmux $ restop do
    stderrLog (Log.info [exon|Waiting for one second before storing new screenshot for '#{name}'|])
    Time.sleep (Seconds 1)
    void (screenshot sane True name pane)

awaitScreenshot' ::
  HasCallStack =>
  Members [NativeTmux, NativeCommandCodecE, Stop CodecError] r =>
  Members [Hedgehog IO, Test, Error TestError, Error Failure, ChronosTime, StderrLog, Race, Embed IO] r =>
  Bool ->
  Bool ->
  Text ->
  Int ->
  Sem r ()
awaitScreenshot' = \case
  True ->
    updateScreeshot
  False ->
    withFrozenCallStack assertScreenshot

awaitScreenshot ::
  HasCallStack =>
  Members [NativeTmux, NativeCommandCodecE, Stop CodecError] r =>
  Members [Hedgehog IO, Test, Error TestError, Error Failure, ChronosTime, StderrLog, Race, Embed IO] r =>
  Bool ->
  Text ->
  Int ->
  Sem r ()
awaitScreenshot record =
  withFrozenCallStack do
    awaitScreenshot' record True
