module Ribosome.Menu.Interpreter.PromptInput where

import Conc (interpretAtomic, race, resultToMaybe)
import Polysemy.Chronos (ChronosTime)
import qualified Polysemy.Time as Time
import qualified Queue
import Time (MilliSeconds (MilliSeconds))

import Ribosome.Host.Api.Effect (vimCallFunction)
import Ribosome.Host.Class.Msgpack.Encode (toMsgpack)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Menu.Effect.PromptControl (PromptControl, waitPromptQuit)
import Ribosome.Menu.Effect.PromptInput (PromptInput (Event))
import Ribosome.Menu.Prompt.Data.Codes (decodeInputChar, decodeInputNum)
import qualified Ribosome.Menu.Prompt.Data.InputEvent as InputEvent
import Ribosome.Menu.Prompt.Data.InputEvent (InputEvent)

interpretPromptInputList ::
  Members [ChronosTime, Embed IO] r =>
  [InputEvent] ->
  InterpreterFor PromptInput r
interpretPromptInputList events =
  interpretAtomic events .
  reinterpret \case
    Event -> do
      maybe (InputEvent.NoInput <$ Time.sleep (MilliSeconds 100)) pure =<< atomicState' \case
        [] -> ([], Nothing)
        (h : t) -> (t, Just h)

interpretPromptInputCharList ::
  Members [ChronosTime, Embed IO] r =>
  [Text] ->
  InterpreterFor PromptInput r
interpretPromptInputCharList cs =
  interpretPromptInputList (InputEvent.Character <$> cs)

quitChar :: Char
quitChar =
  '†'

quitCharOrd :: Int
quitCharOrd =
  ord quitChar

getChar ::
  Members [PromptControl, Rpc !! RpcError, Rpc, Race, Embed IO] r =>
  Sem r InputEvent
getChar =
  resumeAs @RpcError InputEvent.Interrupt getOne
  where
    getOne =
      either pure event =<< race waitQuit (getchar [])
    waitQuit =
      waitPromptQuit *> getchar [toMsgpack True] $> InputEvent.Interrupt
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

interpretPromptInputNvim ::
  Members [PromptControl, Rpc !! RpcError, Rpc, Race, Embed IO] r =>
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
