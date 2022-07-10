module Ribosome.Menu.Interpreter.NvimPromptInput where

import Conc (interpretAtomic, race)
import Exon (exon)
import qualified Log
import Polysemy.Chronos (ChronosTime)

import Ribosome.Host.Api.Effect (vimCallFunction)
import Ribosome.Host.Class.Msgpack.Encode (toMsgpack)
import Ribosome.Host.Data.RpcError (RpcError)
import Ribosome.Host.Effect.Rpc (Rpc)
import Ribosome.Menu.Effect.NvimPromptInput (NvimPromptInput (GetChar))
import Ribosome.Menu.Interpreter.PromptInput (takePromptInputAtomic)
import Ribosome.Menu.Prompt.Data.Codes (decodeInputChar, decodeInputNum)
import qualified Ribosome.Menu.Prompt.Data.InputEvent as InputEvent
import Ribosome.Menu.Prompt.Data.InputEvent (InputEvent)

interpretNvimPromptInputList ::
  Members [ChronosTime, Race, Embed IO] r =>
  [InputEvent] ->
  InterpreterFor NvimPromptInput r
interpretNvimPromptInputList events =
  interpretAtomic events .
  reinterpretH \case
    GetChar waitQuit ->
      pureT . unify =<< race (InputEvent.Interrupt <$ runTSimple waitQuit) (takePromptInputAtomic)

interpretNvimPromptInputCharList ::
  Members [ChronosTime, Race, Embed IO] r =>
  [Text] ->
  InterpreterFor NvimPromptInput r
interpretNvimPromptInputCharList cs =
  interpretNvimPromptInputList (InputEvent.Character <$> cs)

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
