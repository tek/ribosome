{-# OPTIONS_GHC -F -pgmF htfpp #-}

module MenuSpec (htf_thisModulesTests) where

import Conduit (ConduitT, yieldMany)
import Control.Concurrent.MVar.Lifted (modifyMVar_)
import Control.Monad.Trans.Control (MonadBaseControl)
import Test.Framework

import Ribosome.Menu.Data.MenuConfig (MenuConfig(MenuConfig))
import Ribosome.Menu.Data.MenuContent (MenuContent(MenuContent))
import Ribosome.Menu.Data.MenuItem (MenuItem(MenuItem))
import Ribosome.Menu.Data.MenuUpdate (MenuUpdate(MenuUpdate))
import Ribosome.Menu.Data.Prompt (Prompt(Prompt))
import Ribosome.Menu.Data.PromptConfig (PromptConfig(PromptConfig))
import Ribosome.Menu.Data.PromptEvent (PromptEvent)
import qualified Ribosome.Menu.Data.PromptEvent as PromptEvent (PromptEvent(..))
import qualified Ribosome.Menu.Data.PromptState as PromptState (PromptState(..))
import Ribosome.Menu.Prompt (basicTransition)
import Ribosome.Menu.Run (runMenu)
import Ribosome.Menu.Simple (simpleMenu)
import Ribosome.System.Time (sleep)

promptInput ::
  Monad m =>
  ConduitT () PromptEvent m ()
promptInput =
  yieldMany [
    PromptEvent.Character "w",
    PromptEvent.Character "esc",
    PromptEvent.Character "j",
    PromptEvent.Character "a"
    ]

handle ::
  MonadBaseControl IO m =>
  MVar [Prompt] ->
  MenuUpdate ->
  m ()
handle var (MenuUpdate _ (MenuContent _) prompt) =
  modifyMVar_ var $ return . (prompt :)

render ::
  MonadIO m =>
  MenuUpdate ->
  m ()
render (MenuUpdate _ (MenuContent _) _) =
  sleep 0.01

menuItems ::
  Monad m =>
  ConduitT () MenuItem m ()
menuItems =
  yieldMany [
    MenuItem "item1",
    MenuItem "item2",
    MenuItem "item3",
    MenuItem "item4"
    ]

target :: [Prompt]
target =
  uncurry one' <$> [(1, PromptState.Insert), (0, PromptState.Normal), (0, PromptState.Normal), (0, PromptState.Insert)]
  where
    one' c s = Prompt c s ""

test_menu :: IO ()
test_menu = do
  var <- newMVar []
  runMenu (MenuConfig menuItems (simpleMenu (handle var)) render promptConfig) `runReaderT` ()
  assertEqual target =<< readMVar var
  where
    promptConfig =
      PromptConfig promptInput basicTransition (const (return ()))
