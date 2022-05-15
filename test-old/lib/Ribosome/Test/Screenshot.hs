module Ribosome.Test.Screenshot where

import Chiasma.Data.TmuxError (TmuxError)
import Chiasma.Data.TmuxThunk (TmuxThunk)
import qualified Chiasma.Test.Screenshot as Chiasma (screenshot)
import Control.Monad.Catch (MonadMask)
import Control.Monad.Free.Class (MonadFree)
import Hedgehog (TestT, (===))

import Ribosome.Orphans ()
import Ribosome.Test.Orphans ()
import Ribosome.Test.Unit (fixture)
import Ribosome.Tmux.Run (runTmux)
import Ribosome.System.Time (sleep)
import Ribosome.Test.Await (await)

screenshot ::
  MonadFree TmuxThunk m =>
  MonadIO m =>
  Text ->
  Bool ->
  Int ->
  m (Maybe ([Text], [Text]))
screenshot name record pane = do
  storage <- fixture "screenshots"
  Chiasma.screenshot record storage name pane

assertScreenshot ::
  MonadIO m =>
  MonadMask m =>
  Nvim m =>
  Text ->
  Bool ->
  Int ->
  TestT m ()
assertScreenshot name record pane = do
  lift (runTmux (screenshot name record pane)) >>= check
  where
    check (Just (current, existing)) =
      existing === current
    check Nothing =
      return ()

awaitScreenshot ::
  MonadIO m =>
  MonadMask m =>
  Nvim m =>
  Text ->
  Bool ->
  Int ->
  TestT m ()
awaitScreenshot name True pane =
  sleep 1 <* lift (runTmux (screenshot name True pane))
awaitScreenshot name False pane =
  await (const (assertScreenshot name False pane)) unit
