module Ribosome.Menu.Interpreter.PromptInput where

import Conc (interpretAtomic, race, resultToMaybe)
import qualified Queue
import qualified Sync

import Ribosome.Host.Api.Effect (vimCallFunction)
import Ribosome.Host.Class.Msgpack.Encode (toMsgpack)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Menu.Effect.PromptInput (PromptInput (Event))
import Ribosome.Menu.Prompt.Data.Codes (decodeInputChar, decodeInputNum)
import qualified Ribosome.Menu.Prompt.Data.InputEvent as InputEvent
import Ribosome.Menu.Prompt.Data.InputEvent (InputEvent)
import Ribosome.Menu.Prompt.Data.PromptQuit (PromptQuit)

interpretPromptInputList ::
  Member (Embed IO) r =>
  [InputEvent] ->
  InterpreterFor PromptInput r
interpretPromptInputList events =
  interpretAtomic events .
  reinterpret \case
    Event -> do
      atomicState' \case
        [] -> ([], InputEvent.NoInput)
        (h : t) -> (t, h)

interpretPromptInputCharList ::
  Member (Embed IO) r =>
  [Text] ->
  InterpreterFor PromptInput r
interpretPromptInputCharList chars =
  interpretPromptInputList (InputEvent.Character <$> chars)

quitChar :: Char
quitChar =
  '†'

quitCharOrd :: Int
quitCharOrd =
  ord quitChar

getChar ::
  Members [Sync PromptQuit, Rpc !! RpcError, Rpc, Race, Embed IO] r =>
  Sem r InputEvent
getChar =
  resumeAs @RpcError InputEvent.Interrupt getOne
  where
    getOne =
      either pure event =<< race waitQuit (getchar [])
    waitQuit =
      Sync.block *> getchar [toMsgpack True] $> InputEvent.Interrupt
    getchar =
      vimCallFunction "getchar"
    event (Right c) =
      pure (InputEvent.Character (fromMaybe c (decodeInputChar c)))
    event (Left 0) =
      pure InputEvent.NoInput
    event (Left num) | num == quitCharOrd =
      pure InputEvent.Interrupt
    event (Left num) =
      maybe (InputEvent.Unexpected num) InputEvent.Character <$> decodeInputNum num

-- TODO since getchar is called without args, it should be blocking, so is the sleeping pointless?
interpretPromptInputNvim ::
  Members [Sync PromptQuit, Rpc !! RpcError, Rpc, Race, Embed IO] r =>
  InterpreterFor PromptInput r
interpretPromptInputNvim =
  interpret \case
    Event ->
      getChar

interpretPromptInputQueue ::
  Member (Queue InputEvent) r =>
  InterpreterFor PromptInput r
interpretPromptInputQueue =
  interpret \case
    Event ->
      fromMaybe InputEvent.Interrupt . resultToMaybe <$> Queue.read
