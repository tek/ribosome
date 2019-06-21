{-# OPTIONS_GHC -F -pgmF htfpp #-}

module MenuSpec (htf_thisModulesTests) where

import Conduit (ConduitT, yield, yieldMany)
import Control.Concurrent.MVar.Lifted (modifyMVar_)
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.Map.Strict as Map (fromList)
import Test.Framework

import Ribosome.Control.StrictRibosome (StrictRibosome)
import Ribosome.Menu.Data.Menu (Menu(Menu))
import Ribosome.Menu.Data.MenuAction (MenuAction)
import Ribosome.Menu.Data.MenuConfig (MenuConfig(MenuConfig))
import Ribosome.Menu.Data.MenuConsumer (MenuConsumer(MenuConsumer))
import Ribosome.Menu.Data.MenuConsumerAction (MenuConsumerAction)
import qualified Ribosome.Menu.Data.MenuEvent as MenuEvent (MenuEvent(..))
import Ribosome.Menu.Data.MenuItem (MenuItem(MenuItem))
import qualified Ribosome.Menu.Data.MenuItem as MenuItem (MenuItem(_text))
import Ribosome.Menu.Data.MenuRenderEvent (MenuRenderEvent)
import qualified Ribosome.Menu.Data.MenuRenderEvent as MenuRenderEvent (MenuRenderEvent(..))
import Ribosome.Menu.Data.MenuUpdate (MenuUpdate(MenuUpdate))
import Ribosome.Menu.Prompt.Data.Prompt (Prompt(Prompt))
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig(PromptConfig))
import Ribosome.Menu.Prompt.Data.PromptEvent (PromptEvent)
import qualified Ribosome.Menu.Prompt.Data.PromptEvent as PromptEvent (PromptEvent(..))
import qualified Ribosome.Menu.Prompt.Data.PromptState as PromptState (PromptState(..))
import Ribosome.Menu.Prompt.Run (basicTransition, noPromptRenderer)
import Ribosome.Menu.Run (runMenu)
import Ribosome.Menu.Simple (basicMenu, fuzzyMenuItemMatcher, menuContinue, menuQuit, simpleMenu)
import Ribosome.System.Time (sleep)

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
  ConduitT () [MenuItem Text] m ()
menuItems =
  yield . fmap (MenuItem "name")

storePrompt ::
  MonadBaseControl IO m =>
  MVar [Prompt] ->
  MenuUpdate m a Text ->
  m (MenuConsumerAction m a, Menu Text)
storePrompt prompts (MenuUpdate event menu) =
  check event
  where
    check (MenuEvent.PromptChange _ prompt) =
      store prompt
    check (MenuEvent.Mapping _ prompt) =
      store prompt
    check (MenuEvent.Quit _) =
      menuQuit menu
    check _ =
      menuContinue menu
    store prompt =
      modifyMVar_ prompts (return . (prompt :)) *> menuContinue menu

render ::
  MonadIO m =>
  MonadBaseControl IO m =>
  MVar [[MenuItem Text]] ->
  MenuRenderEvent m a Text ->
  m ()
render varItems (MenuRenderEvent.Render _ (Menu _ items _ _ _ _)) = do
  modifyMVar_ varItems (return . (items :))
  sleep 0.01
render _ (MenuRenderEvent.Quit _) =
  return ()

type MenuTestM = StateT (StrictRibosome ()) IO

menuTest ::
  (MenuUpdate MenuTestM a Text -> MenuTestM (MenuAction MenuTestM a, Menu Text)) ->
  [Text] ->
  [Text] ->
  IO [[MenuItem Text]]
menuTest handler items chars = do
  itemsVar <- newMVar []
  void $ runMenu (conf itemsVar) `execStateT` def
  readMVar itemsVar
  where
    conf itemsVar =
      MenuConfig (menuItems items) (MenuConsumer handler) (render itemsVar) promptConfig def
    promptConfig =
      PromptConfig (promptInput chars) basicTransition noPromptRenderer True

promptTest :: [Text] -> [Text] -> IO ([[MenuItem Text]], [Prompt])
promptTest items chars = do
  prompts <- newMVar []
  itemsResult <- menuTest (basicMenu fuzzyMenuItemMatcher (storePrompt prompts)) items chars
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

itemsTarget1 :: [[MenuItem Text]]
itemsTarget1 =
  [
    item <$> ["i2"],
    item <$> ["i1", "i2", "i3", "i4"]
    ]
  where
    item =
      MenuItem "name"

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

itemsTarget :: [MenuItem Text]
itemsTarget =
  [MenuItem "name" "long-item"]

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
  Menu Text ->
  Prompt ->
  m (MenuConsumerAction m a, Menu Text)
exec var m@(Menu _ items _ _ _ _) _ =
  swapMVar var (MenuItem._text <$> items) *> menuQuit m

test_strictMenuExecute :: IO ()
test_strictMenuExecute = do
  var <- newMVar []
  _ <- menuTest (simpleMenu (Map.fromList [("cr", exec var)])) items3 chars3
  assertEqual items3 =<< readMVar var
