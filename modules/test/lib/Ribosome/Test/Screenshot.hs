{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Ribosome.Test.Screenshot where

import Chiasma.Data.TmuxError (TmuxError)
import Chiasma.Data.TmuxThunk (TmuxThunk)
import qualified Chiasma.Test.Screenshot as Chiasma (screenshot)
import Control.Monad.Catch (MonadMask)
import Control.Monad.Free.Class (MonadFree)
import Test.Framework

import Ribosome.Control.Monad.Ribo (MonadRibo, Nvim)
import Ribosome.Orphans ()
import Ribosome.Test.Orphans ()
import Ribosome.Test.Unit (fixture)
import Ribosome.Tmux.Run (runTmux)

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
  AssertM m =>
  MonadIO m =>
  MonadRibo m =>
  MonadDeepError e TmuxError m =>
  MonadMask m =>
  Nvim m =>
  Text ->
  Bool ->
  Int ->
  m ()
assertScreenshot name record pane =
  runTmux (screenshot name record pane) >>= check
  where
    check (Just (current, existing)) =
      gassertEqual existing current
    check Nothing =
      return ()
