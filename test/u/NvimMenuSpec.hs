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
import Ribosome.Menu.Simple (basicMenu, defaultMenu, menuQuit, menuReturn)
import Ribosome.Msgpack.Encode (MsgpackEncode(toMsgpack))
import Ribosome.Nvim.Api.IO (vimCallFunction, vimCommand, vimGetWindows, vimInput)
import Ribosome.System.Time (sleep)
import Ribosome.Test.Tmux (tmuxGuiSpecDef)
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

nvimMenuSpec :: ConduitT () PromptEvent (Ribo () TestError) () -> RiboT ()
nvimMenuSpec source = do
  result <- promptBlocker spec
  gassertEqual (MenuResult.Return (Just "item5")) result
  where
    spec =
      nvimMenu def (menuItems items) (defaultMenu (Map.fromList [("cr", exec)])) (promptConfig source)

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
      syntheticInput (Just 1) ["<c-c>", "<cr>"]

test_nvimMenuInterrupt :: IO ()
test_nvimMenuInterrupt =
  tmuxGuiSpecDef nvimMenuInterruptSpec

defineGetchar ::
  RiboT ()
defineGetchar =
  defineFunction "Getchar" [] body
  where
    body = [
      "try",
      "return call('getchar', a:000)",
      "catch /^Vim:Interrupt/",
      "return 3",
      "catch",
      "return 0",
      "endtry"
      ]

cursorSpec :: RiboT ()
cursorSpec = do
  tid <- fork getter
  promptBlocker $ traverse_ echo ["a", "b", "c", "d", "e", "f", "g", "h"]
  killThread tid
  where
    echo a =
      vimCommand ("echon '" <> a <> "'") *> send *> sleep 1
    send =
      -- return ()
      syntheticInput Nothing ["a"]
    -- redraw =
    --   vimCommand "redraw" *> vimCommand "redrawstatus"
    getter =
      sleep 0.1 *> getChar *> getter

test_cursor :: IO ()
test_cursor =
  tmuxGuiSpecDef cursorSpec
