{-# OPTIONS_GHC -F -pgmF htfpp #-}

module NvimMenuSpec (htf_thisModulesTests) where

import Conduit (ConduitT, yield, yieldMany)
import Control.Concurrent.Lifted (fork, killThread)
import Control.Exception.Lifted (bracket)
import Control.Lens (element, (^?))
import qualified Data.Map.Strict as Map (empty, fromList)
import Test.Framework

import Ribosome.Api.Input (syntheticInput)
import Ribosome.Control.Monad.Ribo (Ribo)
import Ribosome.Menu.Action (menuReturn)
import qualified Ribosome.Menu.Data.FilteredMenuItem as FilteredMenuItem (item)
import Ribosome.Menu.Data.Menu (Menu(Menu))
import Ribosome.Menu.Data.MenuConsumerAction (MenuConsumerAction)
import Ribosome.Menu.Data.MenuItem (MenuItem, simpleMenuItem)
import qualified Ribosome.Menu.Data.MenuItem as MenuItem (text)
import Ribosome.Menu.Data.MenuResult (MenuResult)
import qualified Ribosome.Menu.Data.MenuResult as MenuResult (MenuResult(..))
import Ribosome.Menu.Prompt.Data.Prompt (Prompt(Prompt))
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig(PromptConfig), PromptFlag(StartInsert))
import Ribosome.Menu.Prompt.Data.PromptEvent (PromptEvent)
import qualified Ribosome.Menu.Prompt.Data.PromptEvent as PromptEvent (PromptEvent(..))
import Ribosome.Menu.Prompt.Nvim (getCharC, nvimPromptRenderer)
import Ribosome.Menu.Prompt.Run (basicTransition)
import Ribosome.Menu.Run (nvimMenu)
import Ribosome.Menu.Simple (Mappings, defaultMenu)
import Ribosome.Nvim.Api.IO (vimGetWindows)
import Ribosome.System.Time (sleep)
import Ribosome.Test.Tmux (tmuxSpecDef)
import TestError (RiboT, TestError)

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
  RiboT (MenuResult a)
runNvimMenu maps source =
  nvimMenu def (menuItems items) (defaultMenu maps) (promptConfig source) Nothing

mappings :: Mappings (Ribo () TestError) (Maybe Text) Text
mappings =
  Map.fromList [("cr", exec)]

nvimMenuSpec ::
  ConduitT () PromptEvent (Ribo () TestError) () ->
  RiboT ()
nvimMenuSpec =
  gassertEqual (MenuResult.Return (Just "item4")) <=< runNvimMenu mappings

nvimMenuStrictSpec :: RiboT ()
nvimMenuStrictSpec =
  nvimMenuSpec (promptInput chars)

test_nvimMenuStrict :: IO ()
test_nvimMenuStrict =
  tmuxSpecDef nvimMenuStrictSpec

nativeChars :: [Text]
nativeChars =
  ["i", "t", "e", "<esc>", "k", "<c-k>", "k", "<cr>"]

nvimMenuNativeSpec :: RiboT ()
nvimMenuNativeSpec =
  bracket (fork input) killThread (const $ nvimMenuSpec (getCharC 0.1))
  where
    input =
      syntheticInput (Just 0.2) nativeChars

test_nvimMenuNative :: IO ()
test_nvimMenuNative =
  tmuxSpecDef nvimMenuNativeSpec

nvimMenuInterruptSpec :: RiboT ()
nvimMenuInterruptSpec = do
  gassertEqual MenuResult.Aborted =<< spec
  gassertEqual 1 =<< length <$> vimGetWindows
  where
    spec :: RiboT (MenuResult ())
    spec =
      bracket (fork input) killThread (const run)
    run =
      nvimMenu def (menuItems items) (defaultMenu Map.empty) (promptConfig (getCharC 0.1)) Nothing
    input =
      syntheticInput (Just 0.2) ["<c-c>", "<cr>"]

test_nvimMenuInterrupt :: IO ()
test_nvimMenuInterrupt =
  tmuxSpecDef nvimMenuInterruptSpec

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

nvimMenuNavSpec :: RiboT ()
nvimMenuNavSpec =
  gassertEqual (MenuResult.Return "toem") =<< run
  where
    run =
      bracket (fork input) killThread (const $ runNvimMenu (Map.fromList [("cr", returnPrompt)]) (getCharC 0.1))
    input =
      syntheticInput (Just 0.2) navChars

test_nvimMenuNav :: IO ()
test_nvimMenuNav =
  tmuxSpecDef nvimMenuNavSpec
