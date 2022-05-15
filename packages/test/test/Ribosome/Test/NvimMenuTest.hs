module Ribosome.Test.NvimMenuTest (
  test_nvimMenu,
) where

import Control.Concurrent.Lifted (fork, killThread)
import Control.Exception.Lifted (bracket)
import Control.Lens (element, use, (^?))
import qualified Data.Map.Strict as Map
import Hedgehog ((===))
import qualified Streamly.Internal.Data.Stream.IsStream as Stream
import Streamly.Prelude (SerialT)
import Test.Tasty (TestTree, testGroup)
import TestError (RiboTest, TestError)

import Ribosome.Api.Buffer (bufferContent, buflisted)
import Ribosome.Api.Input (syntheticInput)
import Ribosome.Data.ScratchOptions (ScratchOptions (_maxSize))
import Ribosome.Menu.Action (menuOk, menuResult)
import Ribosome.Menu.Combinators (sortedEntries)
import qualified Ribosome.Menu.Consumer as Consumer
import Ribosome.Menu.Consumer (Mappings)
import Ribosome.Menu.Data.CursorIndex (CursorIndex (CursorIndex))
import qualified Ribosome.Menu.Data.Entry as Entry (item)
import Ribosome.Menu.Data.Entry (intEntries)
import qualified Ribosome.Menu.Data.Menu as Menu
import Ribosome.Menu.Data.MenuConsumer (MenuWidget, MenuWidgetM)
import qualified Ribosome.Menu.Data.MenuData as MenuCursor
import Ribosome.Menu.Data.MenuItem (MenuItem, simpleMenuItem)
import qualified Ribosome.Menu.Data.MenuItem as MenuItem (text)
import qualified Ribosome.Menu.Data.MenuResult as MenuResult
import Ribosome.Menu.Data.MenuResult (MenuResult (Success))
import Ribosome.Menu.Data.MenuState (menuRead, menuWrite)
import Ribosome.Menu.Data.MenuView (MenuView (MenuView))
import Ribosome.Menu.Nvim (computeView, entrySlice)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt (Prompt), PromptText (PromptText))
import Ribosome.Menu.Prompt.Data.PromptConfig (
  PromptConfig (PromptConfig),
  PromptFlag (StartInsert),
  PromptInput (PromptInput),
  )
import qualified Ribosome.Menu.Prompt.Data.PromptInputEvent as PromptInputEvent (PromptInputEvent (..))
import Ribosome.Menu.Prompt.Nvim (getCharStream, nvimPromptRenderer)
import Ribosome.Menu.Prompt.Transition (basicTransition)
import Ribosome.Menu.Run (nvimMenu, staticNvimMenu)
import Ribosome.Host.Api.Effect (bufferGetName, vimGetBuffers, vimGetWindows)
import Ribosome.System.Time (sleep)
import Ribosome.Test.Run (UnitTest, unitTest)
import Ribosome.Test.Tmux (tmuxTestDef)

promptInput ::
  MonadIO m =>
  [Text] ->
  PromptInput m
promptInput chars' =
  PromptInput \ _ -> Stream.fromList (PromptInputEvent.Character <$> chars')

promptInputAfter ::
  MonadIO m =>
  [Text] ->
  PromptInput m
promptInputAfter chars' =
  PromptInput \ _ -> (Stream.fromEffect (sleep 0.2)) *> Stream.delay 0.01 (Stream.fromList (PromptInputEvent.Character <$> chars'))

menuItems ::
  Monad m =>
  [Text] ->
  SerialT m (MenuItem Text)
menuItems =
  Stream.fromList . fmap (simpleMenuItem "name")

chars :: [Text]
chars =
  ["i", "t", "e", "esc", "k", "k", "k", "cr"]

items :: [Text]
items =
  (mappend "item" . show <$> [(1 :: Int)..8])
  ++
  (mappend "its" . show <$> [(5 :: Int)..9])

exec ::
  ∀ m i .
  MonadIO m =>
  MenuWidget m i Text
exec =
  menuWrite do
    CursorIndex s <- use MenuCursor.cursor
    fs <- use sortedEntries
    maybe menuOk menuResult (fs ^? element s . Entry.item . MenuItem.text)

promptConfig ::
  PromptInput (Ribo () TestError) ->
  PromptConfig (Ribo () TestError)
promptConfig source =
  PromptConfig source basicTransition nvimPromptRenderer [StartInsert]

runNvimMenu ::
  Mappings (Ribo () TestError) Text a ->
  PromptInput (Ribo () TestError) ->
  Ribo () TestError (MenuResult a)
runNvimMenu maps source =
  nvimMenu def { _maxSize = Just 4 } (menuItems items) (Consumer.withMappings maps) (promptConfig source)

mappings :: Mappings (Ribo () TestError) Text Text
mappings =
  Map.fromList [("cr", exec)]

nvimMenuTest ::
  PromptInput (Ribo () TestError) ->
  RiboTest ()
nvimMenuTest =
  ((MenuResult.Success "item4") ===) <=< lift . runNvimMenu mappings

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
  bracket (fork input) killThread \ _ -> nvimMenuTest (getCharStream 0.01)
  where
    input =
      syntheticInput (Just 0.01) nativeChars

test_nvimMenuNative :: UnitTest
test_nvimMenuNative =
  tmuxTestDef nvimMenuNativeTest

test_nvimMenuInterrupt :: UnitTest
test_nvimMenuInterrupt =
  tmuxTestDef do
    (MenuResult.Aborted ===) =<< spec
    (1 ===) =<< length <$> vimGetWindows
    where
      spec :: RiboTest (MenuResult ())
      spec =
        lift (bracket (fork input) killThread (const run))
      run =
        nvimMenu def (menuItems items) Consumer.basic (promptConfig (getCharStream 0.01))
      input =
        syntheticInput (Just 0.01) ["<c-c>", "<cr>"]

returnPrompt ::
  Monad m =>
  MenuWidgetM m Text Text
returnPrompt = do
  Prompt _ _ (PromptText text) <- use Menu.prompt
  menuResult text

navChars :: [Text]
navChars =
  ["i", "t", "e", "m", "1", "<bs>", "<esc>", "h", "h", "h", "h", "h", "x", "a", "o", "<cr>"]

nvimMenuNavTest :: RiboTest ()
nvimMenuNavTest =
  ((MenuResult.Success "toem") ===) =<< lift (bracket (fork input) killThread run)
  where
    run _ =
      runNvimMenu (Map.fromList [("cr", menuRead returnPrompt)]) (getCharStream 0.01)
    input =
      syntheticInput (Just 0.01) navChars

test_nvimMenuNav :: UnitTest
test_nvimMenuNav =
  tmuxTestDef nvimMenuNavTest

test_nvimMenuQuit :: UnitTest
test_nvimMenuQuit =
  tmuxTestDef @() @TestError do
    void $ staticNvimMenu def [] Consumer.basic conf
    ([""] ===) =<< traverse bufferGetName =<< filterM buflisted =<< vimGetBuffers
  where
    conf =
      PromptConfig (PromptInput input) basicTransition nvimPromptRenderer []
    input _ =
      Stream.delay 0.5 (Stream.repeat (PromptInputEvent.Character "esc"))

nmenuBot :: MenuView
nmenuBot =
  MenuView 4 0 4 4

nmenuTop :: MenuView
nmenuTop =
  MenuView 9 5 5 0

nmenuMid :: MenuView
nmenuMid =
  MenuView 7 3 5 3

test_viewScrollUp :: UnitTest
test_viewScrollUp =
  MenuView 6 2 6 4 === computeView 6 5 10 nmenuBot

test_viewScrollDown :: UnitTest
test_viewScrollDown =
  MenuView 8 4 4 0 === computeView 4 5 10 nmenuTop

test_viewMoveCursor :: UnitTest
test_viewMoveCursor =
  MenuView 7 3 4 2 === computeView 4 5 10 nmenuMid

test_viewInitial :: UnitTest
test_viewInitial =
  MenuView 0 0 0 0 === computeView 0 1 10 (MenuView 0 0 0 0)

test_entrySlice :: UnitTest
test_entrySlice =
  6 === length (entrySlice ents 25 30)
  where
    ents =
      intEntries (zip (repeat 0) [1..100])

test_menuScrollUp :: UnitTest
test_menuScrollUp =
  tmuxTestDef @() @TestError do
    Success a <- nvimMenu def { _maxSize = Just 4 } (menuItems its) consumer prompt
    4 === length a
  where
    consumer =
      Consumer.withMappings (Map.singleton "cr" content)
    content = do
      [_, mb] <- vimGetBuffers
      menuResult =<< bufferContent mb
    its =
      replicate 100 "item"
    prompt =
      PromptConfig (promptInputAfter (replicate 20 "k" <> ["cr"])) basicTransition nvimPromptRenderer []

test_nvimMenu :: TestTree
test_nvimMenu =
  testGroup "nvim menu" [
    unitTest "pure" test_nvimMenuPure,
    unitTest "native" test_nvimMenuNative,
    unitTest "interrupt" test_nvimMenuInterrupt,
    unitTest "navigation" test_nvimMenuNav,
    unitTest "close scratch when quitting" test_nvimMenuQuit,
    unitTest "new view after scrolling up" test_viewScrollUp,
    unitTest "new view after scrolling down" test_viewScrollDown,
    unitTest "new view after moving cursor" test_viewMoveCursor,
    unitTest "new view after initial render" test_viewInitial,
    unitTest "extract a slice of entries from the score map" test_entrySlice,
    unitTest "scroll up" test_menuScrollUp
  ]
