{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Ribosome.Test.Screenshot where

import Chiasma.Data.TmuxError (TmuxError)
import Chiasma.Data.TmuxThunk (TmuxThunk)
import qualified Chiasma.Test.Screenshot as Chiasma (screenshot)
import Control.Monad.Catch (MonadMask)
import Control.Monad.DeepError (MonadDeepError)
import Control.Monad.Free.Class (MonadFree)
import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import Test.Framework
import Test.Framework.AssertM (AssertM(..))

import Ribosome.Control.Monad.Ribo (MonadRibo, Nvim)
import Ribosome.Test.Orphans ()
import Ribosome.Orphans ()
import Ribosome.Test.Unit (fixture)
import Ribosome.Tmux.Run (runTmux)

screenshot ::
  MonadFree TmuxThunk m =>
  MonadIO m =>
  String ->
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
  String ->
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
