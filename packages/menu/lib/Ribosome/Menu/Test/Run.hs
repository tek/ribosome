module Ribosome.Menu.Test.Run where

import qualified Streamly.Prelude as Stream
import Streamly.Prelude (SerialT)

import Ribosome.Data.ScratchOptions (ScratchOptions)
import Ribosome.Menu.App (MenuApp)
import Ribosome.Menu.Class.MenuState (MenuState (Item))
import Ribosome.Menu.Data.MenuConfig (MenuConfig)
import Ribosome.Menu.Data.MenuEvent (MenuEvent)
import Ribosome.Menu.Data.MenuItem (MenuItem, simpleMenuItemLines)
import Ribosome.Menu.Data.TestMenuConfig (TestMenuConfig (..), confDefault, confSet)
import Ribosome.Menu.Effect.Menu (MenuLoop)
import Ribosome.Menu.Effect.MenuTest (MenuTest, MenuTests)
import Ribosome.Menu.Interpreter.Menu (MenuLoopDeps, MenuLoopIO, interpretMenuDeps)
import Ribosome.Menu.Interpreter.MenuTest (interpretMenuTests)
import Ribosome.Menu.Test.Main (MenuTestIO, NvimMenuTest, headlessTestMenu, nvimTestMenu)

type MenuAppTest s result =
  MenuLoop s ++ [MenuTest (Item s) result, EventConsumer MenuEvent, Reader MenuConfig]

runTestMenu ::
  ∀ i result r .
  Show i =>
  Members MenuLoopIO r =>
  Members [Fail, Final IO] r =>
  InterpretersFor (MenuTests i result : MenuLoopDeps) r
runTestMenu =
  interpretMenuDeps . interpretMenuTests

testStaticMenu ::
  ∀ result s r .
  MenuState s =>
  Members MenuTestIO r =>
  [MenuItem (Item s)] ->
  TestMenuConfig (Item s) ->
  s ->
  MenuApp s r result ->
  InterpretersFor (MenuAppTest s result) r
testStaticMenu items conf initial app =
  runTestMenu .
  subsume .
  subsume .
  headlessTestMenu conf' initial (insertAt @2 <$> app) .
  insertAt @6
  where
    conf' =
      confSet #items (Stream.fromList items) $
      confDefault #initialItems (not (null items)) $
      conf

testStaticNvimMenu ::
  ∀ result s r .
  MenuState s =>
  Members MenuTestIO r =>
  Members NvimMenuTest r =>
  [MenuItem (Item s)] ->
  TestMenuConfig (Item s) ->
  s ->
  ScratchOptions ->
  MenuApp s r result ->
  InterpretersFor (MenuAppTest s result) r
testStaticNvimMenu items conf initial options app =
  runTestMenu .
  subsume .
  subsume .
  nvimTestMenu conf' initial options (insertAt @2 <$> app) .
  insertAt @6
  where
    conf' =
      confSet #items (Stream.fromList items) $
      confDefault #initialItems (not (null items)) $
      -- confDefault #prompt def $
      conf

testStaticNvimMenuSimple ::
  ∀ result s r .
  Item s ~ () =>
  MenuState s =>
  Members MenuTestIO r =>
  Members NvimMenuTest r =>
  [NonEmpty Text] ->
  TestMenuConfig (Item s) ->
  s ->
  ScratchOptions ->
  MenuApp s r result ->
  InterpretersFor (MenuAppTest s result) r
testStaticNvimMenuSimple items =
  testStaticNvimMenu (simpleMenuItemLines () <$> items)

testNativeMenu ::
  ∀ result s r .
  MenuState s =>
  Members MenuTestIO r =>
  Members NvimMenuTest r =>
  SerialT IO (MenuItem (Item s)) ->
  TestMenuConfig (Item s) ->
  s ->
  ScratchOptions ->
  MenuApp s r result ->
  InterpretersFor (MenuAppTest s result) r
testNativeMenu items conf initial options app =
  runTestMenu .
  subsume .
  subsume .
  nvimTestMenu conf' initial options (insertAt @2 <$> app) .
  insertAt @6
  where
    conf' =
      confSet #items items $
      confDefault #nativePrompt True $
      confDefault #prompt def $
      conf
