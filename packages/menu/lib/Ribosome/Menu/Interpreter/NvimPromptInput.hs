module Ribosome.Menu.Interpreter.NvimPromptInput where

import Conc (interpretAtomic, interpretSync, race, subscribeWhile, withAsync_)
import Exon (exon)
import qualified Log
import Polysemy.Chronos (ChronosTime)
import qualified Sync

import Ribosome.Host.Api.Effect (vimCallFunction)
import Ribosome.Host.Class.Msgpack.Encode (toMsgpack)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Effect.Rpc (Rpc)
import qualified Ribosome.Menu.Data.MenuEvent as MenuItems
import Ribosome.Menu.Data.MenuEvent (MenuEvent)
import Ribosome.Menu.Effect.NvimPromptInput (NvimPromptInput (GetChar))
import Ribosome.Menu.Interpreter.PromptInput (takePromptInputAtomic)
import Ribosome.Menu.Prompt.Data.Codes (decodeInputChar, decodeInputNum)
import qualified Ribosome.Menu.Prompt.Data.InputEvent as InputEvent
import Ribosome.Menu.Prompt.Data.InputEvent (InputEvent)

waitForItems ::
  Member (EventConsumer res MenuEvent) r =>
  Sem r ()
waitForItems =
  subscribeWhile (pure . (MenuItems.Exhausted /=))

initialWait ::
  Members [EventConsumer res MenuEvent, Sync ()] r =>
  Bool ->
  Sem r ()
initialWait waitExhausted = do
  when waitExhausted waitForItems
  void (Sync.putTry ())

interpretNvimPromptInputList ::
  Members [EventConsumer res MenuEvent, ChronosTime, Resource, Race, Async, Embed IO] r =>
  Bool ->
  [InputEvent] ->
  InterpreterFor NvimPromptInput r
interpretNvimPromptInputList waitExhausted events =
  interpretSync .
  withAsync_ (initialWait waitExhausted) .
  interpretAtomic events .
  reinterpret2H \case
    GetChar waitQuit -> do
      void Sync.block
      pureT . unify =<< race (InputEvent.Interrupt <$ runTSimple waitQuit) takePromptInputAtomic

promptInputEvents ::
  Members [EventConsumer res MenuEvent, ChronosTime, Resource, Race, Async, Embed IO] r =>
  [InputEvent] ->
  InterpreterFor NvimPromptInput r
promptInputEvents =
  interpretNvimPromptInputList True

interpretNvimPromptInputCharList ::
  Members [EventConsumer res MenuEvent, ChronosTime, Resource, Race, Async, Embed IO] r =>
  Bool ->
  [Text] ->
  InterpreterFor NvimPromptInput r
interpretNvimPromptInputCharList waitExhausted cs =
  interpretNvimPromptInputList waitExhausted (InputEvent.Character <$> cs)

promptInput ::
  Members [EventConsumer res MenuEvent, ChronosTime, Resource, Race, Async, Embed IO] r =>
  [Text] ->
  InterpreterFor NvimPromptInput r
promptInput =
  interpretNvimPromptInputCharList True

quitChar :: Char
quitChar =
  '†'

quitCharOrd :: Int
quitCharOrd =
  ord quitChar

getChar ::
  Members [Rpc !! RpcError, Log, Race, Embed IO] r =>
  Sem (Rpc : r) () ->
  Sem r InputEvent
getChar waitPromptQuit =
  InputEvent.Interrupt <! getOne
  where
    getOne =
      either pure event =<< race waitQuit (getchar [])
    waitQuit = do
      waitPromptQuit
      Log.debug "Prompt has quit"
      InputEvent.Interrupt <$ getchar [toMsgpack True]
    getchar =
      vimCallFunction "getchar"
    event = \case
      Right c -> do
        Log.debug [exon|Nvim prompt char: #{c}|]
        pure (InputEvent.Character (fromMaybe c (decodeInputChar c)))
      Left 0 -> do
        Log.debug "Nvim prompt: zero"
        pure InputEvent.NoInput
      Left num | num == quitCharOrd -> do
        Log.debug "Nvim prompt: quit char"
        pure InputEvent.Interrupt
      Left num -> do
        Log.debug [exon|Nvim prompt numeric: #{show num}|]
        maybe (InputEvent.Unexpected num) InputEvent.Character <$> decodeInputNum num

interpretNvimPromptInput ::
  Members [Rpc !! RpcError, Log, Race, Embed IO] r =>
  InterpreterFor NvimPromptInput r
interpretNvimPromptInput =
  interpretH \case
    GetChar waitQuit ->
      pureT =<< getChar (void (raise (runTSimple waitQuit)))
