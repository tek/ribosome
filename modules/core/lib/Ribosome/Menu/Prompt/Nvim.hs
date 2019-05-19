module Ribosome.Menu.Prompt.Nvim where

import Conduit (ConduitT, yield)
import Control.Exception.Lifted (bracket_)
import qualified Data.Text as Text (singleton, splitAt, uncons)

import Ribosome.Api.Atomic (atomic)
import Ribosome.Api.Function (defineFunction)
import Ribosome.Api.Variable (setVar)
import Ribosome.Control.Monad.Ribo (NvimE)
import Ribosome.Data.Text (escapeQuotes)
import Ribosome.Menu.Prompt.Data.Codes (decodeInputChar, decodeInputNum)
import Ribosome.Menu.Prompt.Data.InputEvent (InputEvent)
import qualified Ribosome.Menu.Prompt.Data.InputEvent as InputEvent (InputEvent(..))
import Ribosome.Menu.Prompt.Data.Prompt (Prompt(Prompt))
import Ribosome.Menu.Prompt.Data.PromptEvent (PromptEvent)
import qualified Ribosome.Menu.Prompt.Data.PromptEvent as PromptEvent (PromptEvent(..))
import Ribosome.Menu.Prompt.Data.PromptRenderer (PromptRenderer(PromptRenderer))
import Ribosome.Msgpack.Encode (toMsgpack)
import Ribosome.Msgpack.Error (DecodeError)
import qualified Ribosome.Nvim.Api.Data as ApiData (vimCommand)
import Ribosome.Nvim.Api.IO (vimCallFunction, vimCommand)
import Ribosome.Nvim.Api.RpcCall (syncRpcCall)
import Ribosome.System.Time (sleep)

getChar ::
  NvimE e m =>
  MonadBaseControl IO m =>
  m InputEvent
getChar =
  event =<< vimCallFunction "getchar" [toMsgpack False]
  where
    event (Right c) =
      return $ InputEvent.Character (fromMaybe c (decodeInputChar c))
    event (Left 0) =
      return InputEvent.NoInput
    event (Left num) =
      maybe (InputEvent.Unexpected num) InputEvent.Character <$> decodeInputNum num

getCharC ::
  MonadIO m =>
  MonadBaseControl IO m =>
  NvimE e m =>
  Double ->
  ConduitT () PromptEvent m ()
getCharC interval =
  recurse
  where
    recurse =
      translate =<< lift getChar
    translate (InputEvent.Character a) =
      yield (PromptEvent.Character a) *> recurse
    translate InputEvent.NoInput =
      sleep interval *> recurse
    translate (InputEvent.Unexpected _) =
      recurse

promptFragment :: Text -> Text -> [Text]
promptFragment hl text =
  ["echohl " <> hl, "echon '" <> escapeQuotes text <> "'"]

nvimRenderPrompt ::
  Monad m =>
  NvimE e m =>
  MonadDeepError e DecodeError m =>
  Prompt ->
  m ()
nvimRenderPrompt (Prompt cursor _ text) =
  void $ atomic calls
  where
    calls = syncRpcCall . ApiData.vimCommand <$> ("redraw" : (fragments >>= uncurry promptFragment))
    fragments =
      [("None", pre), ("RibosomePromptCaret", Text.singleton cursorChar), ("None", post)]
    (pre, rest) = Text.splitAt cursor text
    (cursorChar, post) = fromMaybe (' ', "") (Text.uncons rest)

loopFunctionName :: Text
loopFunctionName =
  "MyoMenuLoop"

loopVarName :: Text
loopVarName =
  "ribosome_menu_looping"

defineLoopFunction ::
  NvimE e m =>
  m ()
defineLoopFunction =
  defineFunction loopFunctionName [] ["while g:" <> loopVarName, "sleep 50m", "endwhile"]

startLoop ::
  NvimE e m =>
  m ()
startLoop = do
  defineLoopFunction
  setVar loopVarName True
  vimCommand $ "call feedkeys(\":call " <> loopFunctionName <> "()\\<cr>\")"

killLoop ::
  NvimE e m =>
  m ()
killLoop = do
  setVar loopVarName False
  vimCommand $ "delfunction! " <> loopFunctionName

promptBlocker ::
  NvimE e m =>
  MonadBaseControl IO m =>
  m a ->
  m a
promptBlocker =
  bracket_ startLoop killLoop

nvimPromptRenderer ::
  Monad m =>
  NvimE e m =>
  MonadDeepError e DecodeError m =>
  PromptRenderer m
nvimPromptRenderer =
  PromptRenderer startLoop (const killLoop) nvimRenderPrompt
