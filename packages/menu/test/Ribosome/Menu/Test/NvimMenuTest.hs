module Ribosome.Menu.Test.NvimMenuTest where

import Control.Concurrent.Lifted (threadDelay)
import Control.Lens (element, use, (^?))
import qualified Data.Map.Strict as Map
import Polysemy.Conc (interpretMaskFinal, withAsync_)
import Polysemy.Conc.Interpreter.Mask (Restoration)
import Polysemy.Test (Hedgehog, TestError, UnitTest, assertEq, runTestAuto, unitTest, (===))
import qualified Polysemy.Time as Time
import Polysemy.Time (MilliSeconds (MilliSeconds), convert)
import qualified Streamly.Internal.Data.Stream.IsStream as Stream
import Streamly.Prelude (SerialT)
import Test.Tasty (TestTree, testGroup)

import Ribosome.Api.Buffer (bufferContent, buflisted)
import Ribosome.Api.Input (syntheticInput)
import Ribosome.Data.PluginName (PluginName)
import Ribosome.Data.ScratchOptions (ScratchOptions (maxSize))
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Effect.Scratch (Scratch)
import Ribosome.Effect.Settings (Settings)
import Ribosome.Host.Api.Effect (bufferGetName, vimGetBuffers, vimGetWindows)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Interpreter.Settings (interpretSettingsRpc)
import Ribosome.Menu.Action (menuOk, menuResult)
import Ribosome.Menu.Combinators (sortedEntries)
import qualified Ribosome.Menu.Consumer as Consumer
import Ribosome.Menu.Consumer (Mappings)
import Ribosome.Menu.Data.CursorIndex (CursorIndex (CursorIndex))
import qualified Ribosome.Menu.Data.Entry as Entry (item)
import Ribosome.Menu.Data.Entry (intEntries)
import qualified Ribosome.Menu.Data.Menu as Menu
import Ribosome.Menu.Data.MenuAction (MenuAction)
import Ribosome.Menu.Data.MenuConsumer (MenuWidget)
import qualified Ribosome.Menu.Data.MenuData as MenuCursor
import Ribosome.Menu.Data.MenuItem (MenuItem, simpleMenuItem)
import qualified Ribosome.Menu.Data.MenuItem as MenuItem (text)
import qualified Ribosome.Menu.Data.MenuResult as MenuResult
import Ribosome.Menu.Data.MenuResult (MenuResult (Success))
import Ribosome.Menu.Data.MenuState (MenuSem, SemS (SemS), menuRead, menuWrite, semState)
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
import Ribosome.Test.Error (resumeTestError)
import Ribosome.Test.Run (embedPluginTest_)

sleep ::
  Double ->
  IO ()
sleep t =
  threadDelay (round (t * 1000000))

promptInputWith ::
  Maybe Double ->
  Maybe Double ->
  SerialT IO Text ->
  PromptInput
promptInputWith delay interval chars =
  PromptInput \ _ -> do
    traverse_ (Stream.fromEffect . sleep) delay
    maybe id Stream.delay interval (PromptInputEvent.Character <$> chars)

promptInput ::
  [Text] ->
  PromptInput
promptInput =
  promptInputWith Nothing Nothing . Stream.fromList

menuItems ::
  Monad m =>
  [Text] ->
  SerialT m (MenuItem Text)
menuItems =
  Stream.fromList . fmap (simpleMenuItem "name")

pureChars :: [Text]
pureChars =
  ["i", "t", "e", "esc", "k", "k", "k", "cr"]

items :: [Text]
items =
  (mappend "item" . show <$> [(1 :: Int)..8])
  <>
  (mappend "its" . show <$> [(5 :: Int)..9])

exec ::
  ∀ i r .
  Members [Resource, Embed IO] r =>
  MenuWidget i r Text
exec =
  menuWrite do
    semState do
      CursorIndex s <- use MenuCursor.cursor
      fs <- use sortedEntries
      SemS (maybe menuOk menuResult (fs ^? element s . Entry.item . MenuItem.text))

promptConfig ::
  Members [Rpc, Rpc !! RpcError, Final IO] r =>
  PromptInput ->
  PromptConfig r
promptConfig source =
  PromptConfig source basicTransition nvimPromptRenderer [StartInsert]

runNvimMenu ::
  Members [Scratch !! RpcError, Rpc !! RpcError, Settings !! SettingError, Error TestError] r =>
  Members [Rpc, Reader PluginName, Log, Resource, Race, Embed IO, Final IO] r =>
  Mappings Text (Scratch : Mask Restoration : r) a ->
  PromptInput ->
  Sem r (MenuResult a)
runNvimMenu maps source =
  interpretMaskFinal $ resumeTestError @Scratch do
    nvimMenu def { maxSize = Just 4 } (menuItems items) (Consumer.withMappings maps) (promptConfig source)

mappings ::
  Members [Resource, Embed IO] r =>
  Mappings Text r Text
mappings =
  Map.fromList [("cr", exec)]

nvimMenuTest ::
  Members [Scratch !! RpcError, Rpc !! RpcError, Settings !! SettingError, Hedgehog IO, Error TestError] r =>
  Members [Rpc, Reader PluginName, Log, Resource, Race, Embed IO, Final IO] r =>
  PromptInput ->
  Sem r ()
nvimMenuTest =
  assertEq (MenuResult.Success "item4") <=< runNvimMenu mappings

test_nvimMenuPure :: UnitTest
test_nvimMenuPure =
  embedPluginTest_ $ interpretSettingsRpc do
    nvimMenuTest (promptInput pureChars)

nativeChars :: [Text]
nativeChars =
  ["i", "t", "e", "<esc>", "k", "<c-k>", "k", "<cr>"]

withInput ::
  Members [Rpc, Resource, Race, Async, Time t d] r =>
  Maybe MilliSeconds ->
  Maybe MilliSeconds ->
  [Text] ->
  Sem r a ->
  Sem r a
withInput delay interval chrs =
  withAsync_ (traverse_ Time.sleep delay *> syntheticInput (convert <$> interval) chrs)

test_nvimMenuNative :: UnitTest
test_nvimMenuNative =
  embedPluginTest_ do
    inp <- getCharStream (MilliSeconds 10)
    withInput Nothing (Just (MilliSeconds 10)) nativeChars do
      nvimMenuTest inp

test_nvimMenuInterrupt :: UnitTest
test_nvimMenuInterrupt =
  embedPluginTest_ $ interpretMaskFinal do
    (MenuResult.Aborted ===) =<< withInput (Just (MilliSeconds 2000)) (Just (MilliSeconds 50)) ["<c-c>", "<cr>"] do
      conf <- promptConfig <$> getCharStream (MilliSeconds 10)
      resumeTestError (nvimMenu @_ @() def (menuItems items) Consumer.basic conf)
    assertEq 1 . length =<< vimGetWindows

returnPrompt ::
  MenuSem Text r (Maybe (MenuAction r Text))
returnPrompt = do
  Prompt _ _ (PromptText text) <- semState (use Menu.prompt)
  menuResult text

navChars :: [Text]
navChars =
  ["i", "t", "e", "m", "1", "<bs>", "<esc>", "h", "h", "h", "h", "h", "x", "a", "o", "<cr>"]

navMappings ::
  Members [Resource, Embed IO] r =>
  Mappings Text r Text
navMappings =
  Map.fromList [("cr", menuRead returnPrompt)]

test_nvimMenuNav :: UnitTest
test_nvimMenuNav =
  embedPluginTest_ do
    assertEq (MenuResult.Success "toem") =<< do
      withInput Nothing (Just (MilliSeconds 10)) navChars do
        runNvimMenu navMappings =<< getCharStream (MilliSeconds 10)

test_nvimMenuQuit :: UnitTest
test_nvimMenuQuit =
  embedPluginTest_ $ interpretMaskFinal $ resumeTestError @Scratch do
    void $ staticNvimMenu def [] Consumer.basic (PromptConfig inp basicTransition nvimPromptRenderer [])
    assertEq [""] =<< traverse bufferGetName =<< filterM buflisted =<< vimGetBuffers
  where
    inp =
      promptInputWith (Just 0.5) Nothing (Stream.repeat "esc")

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
  runTestAuto do
    MenuView 6 2 6 4 === computeView 6 5 10 nmenuBot

test_viewScrollDown :: UnitTest
test_viewScrollDown =
  runTestAuto do
    MenuView 8 4 4 0 === computeView 4 5 10 nmenuTop

test_viewMoveCursor :: UnitTest
test_viewMoveCursor =
  runTestAuto do
    MenuView 7 3 4 2 === computeView 4 5 10 nmenuMid

test_viewInitial :: UnitTest
test_viewInitial =
  runTestAuto do
    MenuView 0 0 0 0 === computeView 0 1 10 (MenuView 0 0 0 0)

test_entrySlice :: UnitTest
test_entrySlice =
  runTestAuto do
    6 === length (entrySlice ents 25 30)
  where
    ents =
      intEntries (zip (repeat 0) [1..100])

test_menuScrollUp :: UnitTest
test_menuScrollUp =
  embedPluginTest_ $ interpretMaskFinal $ resumeTestError @Scratch do
    let prompt = PromptConfig (promptInputWith (Just 0.2) (Just 0.01) chars) basicTransition nvimPromptRenderer []
    Success a <- nvimMenu def { maxSize = Just 4 } (menuItems its) consumer prompt
    4 === length a
  where
    chars =
      Stream.fromList (replicate 20 "k" <> ["cr"])
    consumer =
      Consumer.withMappings (Map.singleton "cr" content)
    content = do
      [_, mb] <- vimGetBuffers
      menuResult =<< bufferContent mb
    its =
      replicate 100 "item"

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
