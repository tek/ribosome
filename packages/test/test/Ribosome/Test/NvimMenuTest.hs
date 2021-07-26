module Ribosome.Test.NvimMenuTest where

import Conduit (ConduitT, yield, yieldMany)
import Control.Concurrent.Lifted (fork, killThread)
import Control.Exception.Lifted (bracket)
import Control.Lens (element, (^?))
import qualified Data.Map.Strict as Map (empty, fromList)
import Hedgehog ((===))
import Test.Tasty (TestTree, testGroup)
import TestError (RiboTest, TestError)

import Ribosome.Api.Input (syntheticInput)
import Ribosome.Control.Monad.Ribo (Ribo)
import Ribosome.Data.ScratchOptions (ScratchOptions (_maxSize))
import Ribosome.Menu.Action (menuReturn)
import qualified Ribosome.Menu.Data.FilteredMenuItem as FilteredMenuItem (item)
import Ribosome.Menu.Data.Menu (Menu (Menu))
import Ribosome.Menu.Data.MenuConsumerAction (MenuConsumerAction)
import Ribosome.Menu.Data.MenuItem (MenuItem, simpleMenuItem)
import qualified Ribosome.Menu.Data.MenuItem as MenuItem (text)
import Ribosome.Menu.Data.MenuResult (MenuResult)
import qualified Ribosome.Menu.Data.MenuResult as MenuResult (MenuResult (..))
import Ribosome.Menu.Prompt.Data.Prompt (Prompt (Prompt))
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig (PromptConfig), PromptFlag (StartInsert))
import Ribosome.Menu.Prompt.Data.PromptEvent (PromptEvent)
import qualified Ribosome.Menu.Prompt.Data.PromptEvent as PromptEvent (PromptEvent (..))
import Ribosome.Menu.Prompt.Nvim (getCharC, nvimPromptRenderer)
import Ribosome.Menu.Prompt.Run (basicTransition)
import Ribosome.Menu.Run (nvimMenu)
import Ribosome.Menu.Simple (Mappings, defaultMenu)
import Ribosome.Nvim.Api.IO (vimGetWindows)
import Ribosome.System.Time (sleep)
import Ribosome.Test.Run (UnitTest, unitTest)
import Ribosome.Test.Tmux (tmuxTestDef)

promptInput ::
  MonadIO m =>
  [Text] ->
  ConduitT () PromptEvent m ()
promptInput chars' = do
  lift $ sleep 0.1
  yieldMany (PromptEvent.Character <$> chars')

menuItems ::
  Monad m =>
  [Text] ->
  ConduitT () [MenuItem Text] m ()
menuItems =
  yield . fmap (simpleMenuItem "name")

chars :: [Text]
chars =
  ["i", "t", "e", "esc", "k", "k", "k", "cr"]

items :: [Text]
items =
  ("item" <>) . show <$> [(1 :: Int)..8]

exec ::
  MonadIO m =>
  Menu Text ->
  Prompt ->
  m (MenuConsumerAction m (Maybe Text), Menu Text)
exec m@(Menu _ items' selected _ _ _) _ =
  menuReturn item m
  where
    item =
      items' ^? element selected . FilteredMenuItem.item . MenuItem.text

promptConfig ::
  ConduitT () PromptEvent (Ribo () TestError) () ->
  PromptConfig (Ribo () TestError)
promptConfig source =
  PromptConfig source basicTransition nvimPromptRenderer [StartInsert]

runNvimMenu ::
  Mappings (Ribo () TestError) a Text ->
  ConduitT () PromptEvent (Ribo () TestError) () ->
  Ribo () TestError (MenuResult a)
runNvimMenu maps source =
  nvimMenu def { _maxSize = Just 4 } (menuItems items) (defaultMenu maps) (promptConfig source) Nothing

mappings :: Mappings (Ribo () TestError) (Maybe Text) Text
mappings =
  Map.fromList [("cr", exec)]

nvimMenuTest ::
  ConduitT () PromptEvent (Ribo () TestError) () ->
  RiboTest ()
nvimMenuTest =
  (MenuResult.Return (Just "item4") ===) <=< lift . runNvimMenu mappings

nvimMenuPureTest :: RiboTest ()
nvimMenuPureTest =
  nvimMenuTest (promptInput chars)

test_nvimMenuPure :: UnitTest
test_nvimMenuPure =
  tmuxTestDef nvimMenuPureTest

nativeChars :: [Text]
nativeChars =
  ["i", "t", "e", "<esc>", "k", "<c-k>", "k", "<cr>"]

nvimMenuNativeTest :: RiboTest ()
nvimMenuNativeTest =
  bracket (fork input) killThread (const $ nvimMenuTest (getCharC 0.1))
  where
    input =
      syntheticInput (Just 0.2) nativeChars

test_nvimMenuNative :: UnitTest
test_nvimMenuNative =
  tmuxTestDef nvimMenuNativeTest

nvimMenuInterruptTest :: RiboTest ()
nvimMenuInterruptTest = do
  (MenuResult.Aborted ===) =<< spec
  (1 ===) =<< length <$> vimGetWindows
  where
    spec :: RiboTest (MenuResult ())
    spec =
      lift (bracket (fork input) killThread (const run))
    run =
      nvimMenu def (menuItems items) (defaultMenu Map.empty) (promptConfig (getCharC 0.1)) Nothing
    input =
      syntheticInput (Just 0.2) ["<c-c>", "<cr>"]

test_nvimMenuInterrupt :: UnitTest
test_nvimMenuInterrupt =
  tmuxTestDef nvimMenuInterruptTest

returnPrompt ::
  MonadIO m =>
  Menu Text ->
  Prompt ->
  m (MenuConsumerAction m Text, Menu Text)
returnPrompt m (Prompt _ _ text) =
  menuReturn text m

navChars :: [Text]
navChars =
  ["i", "t", "e", "m", "1", "<bs>", "<esc>", "h", "h", "h", "h", "h", "x", "a", "o", "<cr>"]

nvimMenuNavTest :: RiboTest ()
nvimMenuNavTest =
  (MenuResult.Return "toem" ===) =<< lift run
  where
    run =
      bracket (fork input) killThread (const $ runNvimMenu (Map.fromList [("cr", returnPrompt)]) (getCharC 0.1))
    input =
      syntheticInput (Just 0.2) navChars

test_nvimMenuNav :: UnitTest
test_nvimMenuNav =
  tmuxTestDef nvimMenuNavTest

test_nvimMenu :: TestTree
test_nvimMenu =
  testGroup "nvim menu" [
    unitTest "pure" test_nvimMenuPure,
    unitTest "native" test_nvimMenuNative,
    unitTest "interrupt" test_nvimMenuInterrupt,
    unitTest "navigation" test_nvimMenuNav
  ]
