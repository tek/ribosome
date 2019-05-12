{-# OPTIONS_GHC -F -pgmF htfpp #-}

module MenuSpec (htf_thisModulesTests) where

import Conduit (ConduitT, yieldMany)
import Control.Concurrent.MVar.Lifted (modifyMVar_)
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.Map as Map (fromList)
import Test.Framework

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
import Ribosome.Menu.Prompt.Run (basicTransition)
import Ribosome.Menu.Run (runMenu)
import Ribosome.Menu.Simple (basicMenu, simpleMenu)
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
  yieldMany . fmap MenuItem

storePrompt ::
  MonadBaseControl IO m =>
  MVar [Prompt] ->
  MenuUpdate ->
  m Menu
storePrompt prompts (MenuUpdate event menu) =
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

menuTest :: (MenuUpdate -> (ReaderT () IO) Menu) -> [Text] -> [Text] -> IO [[MenuItem]]
menuTest handler items chars = do
  itemsVar <- newMVar []
  runMenu (MenuConfig (menuItems items) handler (render itemsVar) promptConfig) `runReaderT` ()
  readMVar itemsVar
  where
    promptConfig =
      PromptConfig (promptInput chars) basicTransition (const unit)

promptTest :: [Text] -> [Text] -> IO ([[MenuItem]], [Prompt])
promptTest items chars = do
  prompts <- newMVar []
  itemsResult <- menuTest (basicMenu (storePrompt prompts)) items chars
  (itemsResult,) <$> readMVar prompts

promptsTarget1 :: [Prompt]
promptsTarget1 =
  uncurry one' <$> [
    (1, PromptState.Insert),
    (0, PromptState.Normal),
    (0, PromptState.Normal),
    (1, PromptState.Insert)
    ]
  where
    one' c s = Prompt c s "i"

items1 :: [Text]
items1 =
  [
    "i1",
    "j1",
    "i2",
    "i3",
    "j2",
    "i4"
    ]

chars1 :: [Text]
chars1 =
  [
    "i",
    "esc",
    "k",
    "a",
    "2"
    ]

itemsTarget1 :: [[MenuItem]]
itemsTarget1 =
  [
    item <$> ["i2"],
    item <$> ["i4", "i3", "i2", "i1"]
    ]
  where
    item =
      MenuItem

test_strictMenuModeChange :: IO ()
test_strictMenuModeChange = do
  (items, prompts) <- promptTest items1 chars1
  assertEqual itemsTarget1 (take 2 items)
  assertEqual promptsTarget1 (take 4 $ reverse prompts)

chars2 :: [Text]
chars2 =
  ["l", "o", "n", "g", "-", "i", "t", "e", "m"]

items2 :: [Text]
items2 =
  [
    "long",
    "short",
    "long-item",
    "longitem"
    ]

itemsTarget :: [MenuItem]
itemsTarget =
  [MenuItem "long-item"]

test_strictMenuFilter :: IO ()
test_strictMenuFilter = do
  items <- fst <$> promptTest items2 chars2
  assertEqual [itemsTarget] (take 1 items)

chars3 :: [Text]
chars3 =
  ["i", "esc", "cr"]

items3 :: [Text]
items3 =
  [
    "item1",
    "item2"
    ]

exec ::
  MonadIO m =>
  MVar [Text] ->
  Menu ->
  Prompt ->
  m Menu
exec var m@(Menu _ items _) _ =
  m <$ swapMVar var (MenuItem.text <$> items)

test_strictMenuExecute :: IO ()
test_strictMenuExecute = do
  var <- newMVar []
  items <- menuTest (simpleMenu (Map.fromList [("cr", exec var)])) items3 chars3
  assertEqual (reverse items3) =<< readMVar var

nvimMenuSpec :: RiboT ()
nvimMenuSpec =
  return ()

test_nvimMenu :: IO ()
test_nvimMenu =
  tmuxGuiSpecDef nvimMenuSpec