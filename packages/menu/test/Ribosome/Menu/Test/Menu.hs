module Ribosome.Menu.Test.Menu where

import Hedgehog.Internal.Property (Failure)
import Polysemy.Chronos (ChronosTime)
import Polysemy.Test (Hedgehog, assertEq)

import Ribosome.Api.Buffer (bufferContent)
import qualified Ribosome.Data.ScratchState
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Menu.Class.MenuState (MenuState)
import Ribosome.Menu.Effect.Menu (Menu)
import qualified Ribosome.Menu.Effect.MenuUi as MenuUi
import Ribosome.Menu.Effect.MenuUi (MenuUi)
import Ribosome.Menu.Items (currentEntriesText)
import Ribosome.Test.Wait (assertWait, (<--))

-- | Wait until the filtered items satisfy the supplied assertion.
assertCurrent ::
  MenuState s =>
  HasCallStack =>
  Members [Menu s, Hedgehog IO, ChronosTime, Error Failure, Race, Async, Embed IO] r =>
  ([Text] -> Sem r a) ->
  Sem r a
assertCurrent target =
  withFrozenCallStack do
    assertWait currentEntriesText target

assertItemCount ::
  MenuState s =>
  HasCallStack =>
  Members [Menu s, Hedgehog IO, ChronosTime, Error Failure, Race, Async, Embed IO] r =>
  Int ->
  Sem r ()
assertItemCount n =
  withFrozenCallStack do
    assertCurrent \ i -> assertEq n (length i)

-- | Wait until the filtered items equal the supplied list.
awaitCurrent ::
  MenuState s =>
  HasCallStack =>
  Members [Menu s, Hedgehog IO, ChronosTime, Error Failure, Race, Async, Embed IO] r =>
  [Text] ->
  Sem r ()
awaitCurrent target =
  withFrozenCallStack do
    assertCurrent (assertEq target)

itemsBufferContent ::
  Members [MenuUi, Rpc] r =>
  Sem r [Text]
itemsBufferContent = do
  items <- MenuUi.itemsScratch
  bufferContent items.buffer

awaitItemsBuffer ::
  HasCallStack =>
  Members [MenuUi, Rpc, Hedgehog IO, ChronosTime, Race, Error Failure, Embed IO] r =>
  [Text] ->
  Sem r ()
awaitItemsBuffer target =
  withFrozenCallStack do
    target <-- itemsBufferContent
