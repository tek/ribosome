module Ribosome.Menu.Prompt.Run where

import Conc (Gates, consumeElem, subscribeAsync, withAsync_)
import Time (MilliSeconds (MilliSeconds), convert, sleep)

import Ribosome.Api.Input (feedKey, syntheticInput, syntheticInputFk)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Menu.Data.MenuConfig (MenuConfig, menuSync)
import Ribosome.Menu.Data.MenuEvent (MenuEvent (PromptLoop))
import Ribosome.Menu.Effect.Menu (MenuCore, MenuEngine, menuEngine, waitPrompt)
import Ribosome.Menu.Prompt.Data.Prompt (Prompt (Prompt))
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptConfig, isStartInsert)
import qualified Ribosome.Menu.Prompt.Data.PromptMode as PromptMode (PromptMode (..))

pristinePrompt :: PromptConfig -> Prompt
pristinePrompt conf =
  Prompt 0 (if (isStartInsert conf) then PromptMode.Insert else PromptMode.Normal) ""

withPromptInput ::
  Members [MenuCore, Rpc, Resource, Race, Async, Time t d] r =>
  Maybe MilliSeconds ->
  [Text] ->
  Sem r a ->
  Sem r a
withPromptInput interval chrs =
  withAsync_ (waitPrompt *> syntheticInput (convert <$> interval) chrs)

data SyncChar =
  SyncChar {
    char :: Text,
    sync :: Bool
  }
  deriving stock (Eq, Show, Generic)

instance IsString SyncChar where
  fromString c =
    SyncChar (fromString c) True

nosync :: SyncChar -> SyncChar
nosync =
  #sync .~ False

withPromptInputSync ::
  Members [Reader MenuConfig, MenuEngine s] r =>
  Members [Rpc, Gates, EventConsumer MenuEvent, Resource, Race, Async, Time t d] r =>
  [SyncChar] ->
  Sem r a ->
  Sem r a
withPromptInputSync chrs =
  menuSync . subscribeAsync do
    menuEngine @MenuCore waitPrompt
    for_ chrs \ (SyncChar {..}) -> do
      sleep (MilliSeconds 1)
      void (feedKey char)
      when sync (consumeElem PromptLoop)

withPromptInputFk ::
  Members [MenuCore, Rpc, Resource, Race, Async, Time t d] r =>
  Maybe MilliSeconds ->
  [Text] ->
  Sem r a ->
  Sem r a
withPromptInputFk interval chrs =
  withAsync_ (waitPrompt *> syntheticInputFk (convert <$> interval) chrs)
