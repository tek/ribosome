{-# OPTIONS_GHC -F -pgmF htfpp #-}

module MenuSpec (htf_thisModulesTests) where

import Conduit (ConduitT, yieldMany)
import Control.Concurrent.MVar.Lifted (modifyMVar_)
import Control.Monad.Trans.Control (MonadBaseControl)
import Test.Framework

import Ribosome.Menu.Data.Menu (Menu(Menu))
import Ribosome.Menu.Data.MenuConfig (MenuConfig(MenuConfig))
import qualified Ribosome.Menu.Data.MenuEvent as MenuEvent (MenuEvent(..))
import Ribosome.Menu.Data.MenuItem (MenuItem(MenuItem))
import Ribosome.Menu.Data.MenuUpdate (MenuUpdate(MenuUpdate))
import Ribosome.Menu.Prompt.Data.Prompt (Prompt(Prompt))
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig(PromptConfig))
import Ribosome.Menu.Prompt.Data.PromptEvent (PromptEvent)
import qualified Ribosome.Menu.Prompt.Data.PromptEvent as PromptEvent (PromptEvent(..))
import qualified Ribosome.Menu.Prompt.Data.PromptState as PromptState (PromptState(..))
import Ribosome.Menu.Prompt.Run (basicTransition)
import Ribosome.Menu.Run (runMenu)
import Ribosome.Menu.Simple (simpleMenu)
import Ribosome.System.Time (sleep)
import Ribosome.Test.Tmux (tmuxGuiSpecDef)
import TestError (RiboT)

promptInput ::
  MonadIO m =>
  ConduitT () PromptEvent m ()
promptInput = do
  lift $ sleep 0.1
  yieldMany [
    PromptEvent.Character "i",
    PromptEvent.Character "esc",
    PromptEvent.Character "k",
    PromptEvent.Character "a",
    PromptEvent.Character "2"
    ]

handle ::
  MonadBaseControl IO m =>
  MVar [Prompt] ->
  MenuUpdate ->
  m Menu
handle prompts (MenuUpdate event menu) =
  check event
  where
    check (MenuEvent.PromptChange _ prompt) =
      store prompt
    check (MenuEvent.Mapping _ prompt) =
      store prompt
    check _ =
      return menu
    store prompt =
      menu <$ modifyMVar_ prompts (return . (prompt :))

render ::
  MonadIO m =>
  MonadBaseControl IO m =>
  MVar [[MenuItem]] ->
  MenuUpdate ->
  m ()
render varItems (MenuUpdate _ (Menu _ items _)) = do
  modifyMVar_ varItems (return . (items :))
  sleep 0.01

menuItems ::
  Monad m =>
  ConduitT () MenuItem m ()
menuItems =
  yieldMany [
    MenuItem "i1",
    MenuItem "j1",
    MenuItem "i2",
    MenuItem "i3",
    MenuItem "j2",
    MenuItem "i4"
    ]

promptsTarget :: [Prompt]
promptsTarget =
  uncurry one' <$> [
    (1, PromptState.Insert),
    (0, PromptState.Normal),
    (0, PromptState.Normal),
    (1, PromptState.Insert)
    ]
  where
    one' c s = Prompt c s "i"

itemsTarget :: [[MenuItem]]
itemsTarget =
  [
    item <$> ["i2"],
    item <$> ["i4", "i3", "i2", "i1"]
    ]
  where
    item =
      MenuItem

test_strictMenu :: IO ()
test_strictMenu = do
  prompts <- newMVar []
  items <- newMVar []
  runMenu (MenuConfig menuItems (simpleMenu (handle prompts)) (render items) promptConfig) `runReaderT` ()
  assertEqual itemsTarget =<< take 2 <$> readMVar items
  assertEqual promptsTarget =<< take 4 . reverse <$> readMVar prompts
  where
    promptConfig =
      PromptConfig promptInput basicTransition (const (return ()))

nvimMenuSpec :: RiboT ()
nvimMenuSpec =
  return ()

test_nvimMenu :: IO ()
test_nvimMenu =
  tmuxGuiSpecDef nvimMenuSpec
