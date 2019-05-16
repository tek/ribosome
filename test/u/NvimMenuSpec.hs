{-# OPTIONS_GHC -F -pgmF htfpp #-}

module NvimMenuSpec (htf_thisModulesTests) where

import Conduit (ConduitT, yieldMany)
import Control.Concurrent.MVar.Lifted (modifyMVar_)
import Control.Lens ((^?))
import qualified Control.Lens as Lens (element)
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.Map as Map (fromList)
import Test.Framework

import Ribosome.Control.Monad.Ribo (Ribo(Ribo), runRibo)
import Ribosome.Menu.Data.Menu (Menu(Menu))
import Ribosome.Menu.Data.MenuConfig (MenuConfig(MenuConfig))
import qualified Ribosome.Menu.Data.MenuEvent as MenuEvent (MenuEvent(..))
import Ribosome.Menu.Data.MenuItem (MenuItem(MenuItem))
import qualified Ribosome.Menu.Data.MenuItem as MenuItem (MenuItem(text))
import Ribosome.Menu.Data.MenuUpdate (MenuUpdate(MenuUpdate))
import Ribosome.Menu.Prompt.Data.Prompt (Prompt(Prompt))
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig(PromptConfig))
import Ribosome.Menu.Prompt.Data.PromptEvent (PromptEvent)
import qualified Ribosome.Menu.Prompt.Data.PromptEvent as PromptEvent (PromptEvent(..))
import qualified Ribosome.Menu.Prompt.Data.PromptState as PromptState (PromptState(..))
import Ribosome.Menu.Prompt.Nvim (nvimRenderPrompt)
import Ribosome.Menu.Prompt.Run (basicTransition)
import Ribosome.Menu.Run (nvimMenu, runMenu)
import Ribosome.Menu.Simple (basicMenu, defaultMenu)
import Ribosome.Nvim.Api.IO (vimCommand)
import Ribosome.System.Time (sleep)
import Ribosome.Test.Tmux (tmuxGuiSpecDef)
import TestError (RiboT)

promptInput ::
  MonadIO m =>
  [Text] ->
  ConduitT () PromptEvent m ()
promptInput chars = do
  lift $ sleep 0.1
  yieldMany (PromptEvent.Character <$> chars)

menuItems ::
  Monad m =>
  [Text] ->
  ConduitT () MenuItem m ()
menuItems =
  yieldMany . fmap (MenuItem "name")

chars :: [Text]
chars =
  ["i", "t", "e", "esc", "k", "k", "k", "cr"]

items :: [Text]
items =
  ("item" <>) . show <$> [1..8]

exec ::
  MonadIO m =>
  MVar (Maybe Text) ->
  Menu ->
  Prompt ->
  m Menu
exec var m@(Menu _ items _ selected) _ =
  m <$ swapMVar var (MenuItem.text <$> item)
  where
    item =
      items ^? Lens.element selected

nvimMenuStrictSpec :: RiboT ()
nvimMenuStrictSpec = do
  vimCommand "highlight link RibosomePromptCaret TermCursor"
  var <- newMVar Nothing
  Ribo $ lift $ nvimMenu def (menuItems items) (defaultMenu (Map.fromList [("cr", exec var)])) promptConfig
  gassertEqual (Just "item5") =<< liftIO (readMVar var)
  where
    promptConfig =
      PromptConfig (promptInput chars) basicTransition nvimRenderPrompt

test_nvimMenuStrict :: IO ()
test_nvimMenuStrict =
  tmuxGuiSpecDef nvimMenuStrictSpec
