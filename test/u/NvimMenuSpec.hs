{-# OPTIONS_GHC -F -pgmF htfpp #-}

module NvimMenuSpec (htf_thisModulesTests) where

import Conduit (ConduitT, yieldMany)
import Control.Concurrent.Lifted (fork, killThread)
import Control.Concurrent.MVar.Lifted (modifyMVar_)
import Control.Exception.Lifted (bracket)
import Control.Lens ((^?))
import qualified Control.Lens as Lens (element)
import qualified Data.Map as Map (fromList)
import qualified Data.Text as Text (unlines)
import Test.Framework

import Ribosome.Api.Variable (setVar)
import Ribosome.Control.Monad.Ribo (Ribo(Ribo), runRibo)
import Ribosome.Error.Report.Class (ReportError)
import Ribosome.Menu.Data.Menu (Menu(Menu))
import Ribosome.Menu.Data.MenuConfig (MenuConfig(MenuConfig))
import Ribosome.Menu.Data.MenuConsumerAction (MenuConsumerAction)
import qualified Ribosome.Menu.Data.MenuEvent as MenuEvent (MenuEvent(..))
import Ribosome.Menu.Data.MenuItem (MenuItem(MenuItem))
import qualified Ribosome.Menu.Data.MenuItem as MenuItem (MenuItem, text)
import qualified Ribosome.Menu.Data.MenuResult as MenuResult (MenuResult(..))
import Ribosome.Menu.Data.MenuUpdate (MenuUpdate(MenuUpdate))
import Ribosome.Menu.Prompt.Data.Prompt (Prompt(Prompt))
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig(PromptConfig))
import Ribosome.Menu.Prompt.Data.PromptEvent (PromptEvent)
import qualified Ribosome.Menu.Prompt.Data.PromptEvent as PromptEvent (PromptEvent(..))
import qualified Ribosome.Menu.Prompt.Data.PromptState as PromptState (PromptState(..))
import Ribosome.Menu.Prompt.Nvim (getCharC, nvimPromptRenderer, promptBlocker)
import Ribosome.Menu.Prompt.Run (basicTransition)
import Ribosome.Menu.Run (nvimMenu, runMenu)
import Ribosome.Menu.Simple (basicMenu, defaultMenu, menuQuit, menuReturn)
import Ribosome.Msgpack.Encode (MsgpackEncode(toMsgpack))
import Ribosome.Nvim.Api.IO (vimCallFunction, vimCommand, vimInput)
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

nvimMenuSpec :: ConduitT () PromptEvent (Ribo () TestError) () -> RiboT ()
nvimMenuSpec source = do
  vimCommand "highlight link RibosomePromptCaret TermCursor"
  var <- newMVar Nothing
  result <- nvimMenu def (menuItems items) (defaultMenu (Map.fromList [("cr", exec)])) promptConfig
  gassertEqual (MenuResult.Return (Just "item5")) result
  where
    promptConfig =
      PromptConfig source basicTransition nvimPromptRenderer True

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
    input = do
      sleep 0.1
      traverse_ vimInput nativeChars
    cleanup inputThreadId =
      killThread inputThreadId

test_nvimMenuNative :: IO ()
test_nvimMenuNative =
  tmuxGuiSpecDef nvimMenuNativeSpec
