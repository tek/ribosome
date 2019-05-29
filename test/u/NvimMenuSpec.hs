{-# OPTIONS_GHC -F -pgmF htfpp #-}

module NvimMenuSpec (htf_thisModulesTests) where

import Conduit (ConduitT, yieldMany)
import Control.Concurrent.Lifted (fork, killThread)
import Control.Concurrent.MVar.Lifted (modifyMVar_)
import Control.Exception.Lifted (bracket)
import Control.Lens ((^?))
import qualified Control.Lens as Lens (element)
import qualified Data.Map as Map (empty, fromList)
import qualified Data.Text as Text (unlines)
import Test.Framework

import Ribosome.Api.Function (defineFunction)
import Ribosome.Api.Input (syntheticInput)
import Ribosome.Api.Variable (setVar)
import Ribosome.Control.Monad.Ribo (NvimE, Ribo(Ribo), runRibo)
import Ribosome.Error.Report.Class (ReportError)
import Ribosome.Menu.Data.Menu (Menu(Menu))
import Ribosome.Menu.Data.MenuConfig (MenuConfig(MenuConfig))
import Ribosome.Menu.Data.MenuConsumerAction (MenuConsumerAction)
import qualified Ribosome.Menu.Data.MenuEvent as MenuEvent (MenuEvent(..))
import Ribosome.Menu.Data.MenuItem (MenuItem(MenuItem))
import qualified Ribosome.Menu.Data.MenuItem as MenuItem (MenuItem, text)
import Ribosome.Menu.Data.MenuResult (MenuResult)
import qualified Ribosome.Menu.Data.MenuResult as MenuResult (MenuResult(..))
import Ribosome.Menu.Data.MenuUpdate (MenuUpdate(MenuUpdate))
import Ribosome.Menu.Prompt.Data.Prompt (Prompt(Prompt))
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig(PromptConfig))
import Ribosome.Menu.Prompt.Data.PromptEvent (PromptEvent)
import qualified Ribosome.Menu.Prompt.Data.PromptEvent as PromptEvent (PromptEvent(..))
import qualified Ribosome.Menu.Prompt.Data.PromptState as PromptState (PromptState(..))
import Ribosome.Menu.Prompt.Nvim (getChar, getCharC, nvimPromptRenderer, promptBlocker)
import Ribosome.Menu.Prompt.Run (basicTransition)
import Ribosome.Menu.Run (nvimMenu, runMenu)
import Ribosome.Menu.Simple (Mappings, basicMenu, defaultMenu, menuQuit, menuReturn)
import Ribosome.Msgpack.Encode (MsgpackEncode(toMsgpack))
import Ribosome.Nvim.Api.IO (vimCallFunction, vimCommand, vimGetWindows, vimInput)
import Ribosome.System.Time (sleep)
import Ribosome.Test.Await (await)
import Ribosome.Test.Screenshot (assertScreenshot)
import Ribosome.Test.Tmux (tmuxGuiSpecDef)
import Ribosome.Test.Unit (withLog)
import TestError (RiboT, TestError)

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
  Menu ->
  Prompt ->
  m (MenuConsumerAction m (Maybe Text), Menu)
exec m@(Menu _ items _ selected _) _ =
  menuReturn item m
  where
    item =
      items ^? Lens.element selected . MenuItem.text

promptConfig ::
  ConduitT () PromptEvent (Ribo () TestError) () ->
  PromptConfig (Ribo () TestError)
promptConfig source =
  PromptConfig source basicTransition nvimPromptRenderer True

runNvimMenu ::
  Mappings (Ribo () TestError) a ->
  ConduitT () PromptEvent (Ribo () TestError) () ->
  RiboT (MenuResult a)
runNvimMenu mappings source =
  nvimMenu def (menuItems items) (defaultMenu mappings) (promptConfig source)

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
      syntheticInput (Just 1) nativeChars

test_nvimMenuNative :: IO ()
test_nvimMenuNative =
  tmuxGuiSpecDef (withLog nvimMenuNativeSpec)

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
      syntheticInput (Just 1) ["<c-c>", "<cr>"]

test_nvimMenuInterrupt :: IO ()
test_nvimMenuInterrupt =
  tmuxGuiSpecDef nvimMenuInterruptSpec

returnPrompt ::
  MonadIO m =>
  Menu ->
  Prompt ->
  m (MenuConsumerAction m Text, Menu)
returnPrompt m@(Menu _ items _ selected _) (Prompt _ _ text) =
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
