module Ribosome.Menu.Test.Loop where

import Conc (timeout_, withAsync_)
import Exon (exon)
import qualified Log

import Ribosome.Menu.App (PromptApp, hoistPromptApp)
import Ribosome.Menu.Class.MenuState (MenuState (Item))
import Ribosome.Menu.Data.MenuEvent (MenuEvent (Query, Rendered), QueryEvent (Refined))
import Ribosome.Menu.Data.TestMenuConfig (TestMenuConfig (..), TestTimeout (TestTimeout))
import Ribosome.Menu.Effect.Menu (MenuLoop, Menus, waitPrompt)
import qualified Ribosome.Menu.Effect.MenuTest as MenuTest
import Ribosome.Menu.Effect.MenuTest (MenuTest, waitEvents)
import Ribosome.Menu.Interpreter.MenuTest (interceptMenuQueue)
import Ribosome.Menu.Loop (menuLoop, menuParams)

testMenuLoop ::
  Members [MenuTest i result, Log] r =>
  Members (MenuLoop s) r =>
  PromptApp s r result ->
  Sem r ()
testMenuLoop app = do
  conf <- MenuTest.config
  let
    promptInterceptor | fromMaybe False conf.nativePrompt = id
                      | otherwise = interceptMenuQueue
  promptInterceptor do
    result <- menuLoop app
    void (MenuTest.loopFinished result)

withMenuLoop ::
  ∀ result s r a .
  Members [MenuTest (Item s) result, Log, Resource, Fail, Race, Async] r =>
  Members (MenuLoop s) r =>
  TestMenuConfig (Item s) ->
  PromptApp s r result ->
  Sem r a ->
  Sem r a
withMenuLoop conf app main = do
  Log.debug [exon|Test config: #{show conf}|]
  TestTimeout timeout <- MenuTest.timeout
  withAsync_ (testMenuLoop app) do
    timeout_ (fail "prompt didn't start") timeout waitPrompt
    waitEvents "initial prompt update and renders" initialEvents
    main
  where
    initialEvents = [Query Refined, Rendered] <> if initialItems then [Rendered] else []
    initialItems = fromMaybe True conf.initialItems

promptTestApp ::
  ∀ result s r .
  Members [Menus s, MenuTest (Item s) result, Log, Resource, Fail, Race, Async] r =>
  TestMenuConfig (Item s) ->
  s ->
  PromptApp s r result ->
  InterpretersFor (MenuLoop s) r
promptTestApp conf initial app main = do
  items <- MenuTest.itemsStream
  menuParams items initial do
    withMenuLoop conf (hoistPromptApp (insertAt @2) app) main
