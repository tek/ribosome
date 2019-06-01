{-# OPTIONS_GHC -F -pgmF htfpp #-}

module NvimMenuSpec (htf_thisModulesTests) where

import Conduit (ConduitT, yieldMany)
import Control.Concurrent.Lifted (fork, killThread)
import Control.Exception.Lifted (bracket)
import Control.Lens ((^?))
import qualified Control.Lens as Lens (element)
import qualified Data.Map as Map (empty, fromList)
import Test.Framework

import Ribosome.Api.Input (syntheticInput)
import Ribosome.Control.Monad.Ribo (Ribo)
import Ribosome.Menu.Data.Menu (Menu(Menu))
import Ribosome.Menu.Data.MenuConsumerAction (MenuConsumerAction)
import Ribosome.Menu.Data.MenuItem (MenuItem(MenuItem))
import qualified Ribosome.Menu.Data.MenuItem as MenuItem (text)
import Ribosome.Menu.Data.MenuResult (MenuResult)
import qualified Ribosome.Menu.Data.MenuResult as MenuResult (MenuResult(..))
import Ribosome.Menu.Prompt.Data.Prompt (Prompt(Prompt))
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig(PromptConfig))
import Ribosome.Menu.Prompt.Data.PromptEvent (PromptEvent)
import qualified Ribosome.Menu.Prompt.Data.PromptEvent as PromptEvent (PromptEvent(..))
import Ribosome.Menu.Prompt.Nvim (getCharC, nvimPromptRenderer)
import Ribosome.Menu.Prompt.Run (basicTransition)
import Ribosome.Menu.Run (nvimMenu)
import Ribosome.Menu.Simple (Mappings, defaultMenu, menuReturn)
import Ribosome.Nvim.Api.IO (vimGetWindows)
import Ribosome.System.Time (sleep)
import Ribosome.Test.Tmux (tmuxGuiSpecDef)
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
  ConduitT () MenuItem m ()
menuItems =
  yieldMany . fmap (MenuItem "name")

chars :: [Text]
chars =
  ["i", "t", "e", "esc", "k", "k", "k", "cr"]

items :: [Text]
items =
  ("item" <>) . show <$> [(1 :: Int)..8]

exec ::
  MonadIO m =>
  Menu ->
  Prompt ->
  m (MenuConsumerAction m (Maybe Text), Menu)
exec m@(Menu _ items' _ selected _) _ =
  menuReturn item m
  where
    item =
      items' ^? Lens.element selected . MenuItem.text

promptConfig ::
  ConduitT () PromptEvent (Ribo () TestError) () ->
  PromptConfig (Ribo () TestError)
promptConfig source =
  PromptConfig source basicTransition nvimPromptRenderer True

runNvimMenu ::
  Mappings (Ribo () TestError) a ->
  ConduitT () PromptEvent (Ribo () TestError) () ->
  RiboT (MenuResult a)
runNvimMenu maps source =
  nvimMenu def (menuItems items) (defaultMenu maps) (promptConfig source)

mappings :: Mappings (Ribo () TestError) (Maybe Text)
mappings =
  Map.fromList [("cr", exec)]

nvimMenuSpec ::
  ConduitT () PromptEvent (Ribo () TestError) () ->
  RiboT ()
nvimMenuSpec =
  gassertEqual (MenuResult.Return (Just "item5")) <=< runNvimMenu mappings

nvimMenuStrictSpec :: RiboT ()
nvimMenuStrictSpec =
  nvimMenuSpec (promptInput chars)

test_nvimMenuStrict :: IO ()
test_nvimMenuStrict =
  tmuxGuiSpecDef nvimMenuStrictSpec

nativeChars :: [Text]
nativeChars =
  ["i", "t", "e", "<esc>", "k", "k", "k", "<cr>"]

nvimMenuNativeSpec :: RiboT ()
nvimMenuNativeSpec =
  bracket (fork input) killThread (const $ nvimMenuSpec (getCharC 0.1))
  where
    input =
      syntheticInput (Just 0.2) nativeChars

test_nvimMenuNative :: IO ()
test_nvimMenuNative =
  tmuxGuiSpecDef nvimMenuNativeSpec

nvimMenuInterruptSpec :: RiboT ()
nvimMenuInterruptSpec = do
  gassertEqual MenuResult.Aborted =<< spec
  gassertEqual 1 =<< length <$> vimGetWindows
  where
    spec :: RiboT (MenuResult ())
    spec =
      bracket (fork input) killThread (const run)
    run =
      nvimMenu def (menuItems items) (defaultMenu Map.empty) (promptConfig (getCharC 0.1))
    input =
      syntheticInput (Just 0.2) ["<c-c>", "<cr>"]

test_nvimMenuInterrupt :: IO ()
test_nvimMenuInterrupt =
  tmuxGuiSpecDef nvimMenuInterruptSpec

returnPrompt ::
  MonadIO m =>
  Menu ->
  Prompt ->
  m (MenuConsumerAction m Text, Menu)
returnPrompt m@(Menu _ _ _ _ _) (Prompt _ _ text) =
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
  tmuxGuiSpecDef nvimMenuNavSpec
