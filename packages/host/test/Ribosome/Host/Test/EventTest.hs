module Ribosome.Host.Test.EventTest where

import Conc (interpretSync, withAsync_)
import qualified Polysemy.Conc.Sync as Sync
import Polysemy.Test (UnitTest, assertJust)
import Polysemy.Time (Seconds (Seconds))

import qualified Ribosome.Host.Api.Data as Data
import Ribosome.Host.Class.Msgpack.Encode (toMsgpack)
import Ribosome.Host.Data.Event (Event (Event))
import qualified Ribosome.Host.Effect.Rpc as Rpc
import Ribosome.Host.Embed (embedNvim_)
import Ribosome.Host.Unit.Run (runTest)

listenEvent ::
  Members [EventConsumer Event, Sync Event] r =>
  Sem r ()
listenEvent =
  subscribe do
    void . Sync.putWait (Seconds 5) =<< consume @Event

target :: Event
target =
  Event "nvim_error_event" [toMsgpack (0 :: Int), toMsgpack ("Vim(write):E32: No file name" :: Text)]

test_errorEvent :: UnitTest
test_errorEvent =
  runTest $ interpretSync $ embedNvim_ do
    withAsync_ listenEvent do
      Rpc.notify (Data.nvimCommand "write")
      assertJust target =<< Sync.wait (Seconds 5)
