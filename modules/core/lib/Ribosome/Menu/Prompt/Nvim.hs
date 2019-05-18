module Ribosome.Menu.Prompt.Nvim where

import Conduit (ConduitT, yield)
import qualified Data.Text as Text (singleton, splitAt, uncons)

import Ribosome.Api.Atomic (atomic)
import Ribosome.Control.Monad.Ribo (NvimE)
import Ribosome.Data.Text (escapeQuotes)
import Ribosome.Menu.Prompt.Data.Codes (decodeInputChar, decodeInputNum)
import Ribosome.Menu.Prompt.Data.InputEvent (InputEvent)
import qualified Ribosome.Menu.Prompt.Data.InputEvent as InputEvent (InputEvent(..))
import Ribosome.Menu.Prompt.Data.Prompt (Prompt(Prompt))
import Ribosome.Menu.Prompt.Data.PromptEvent (PromptEvent)
import qualified Ribosome.Menu.Prompt.Data.PromptEvent as PromptEvent (PromptEvent(..))
import Ribosome.Msgpack.Encode (toMsgpack)
import Ribosome.Msgpack.Error (DecodeError)
import qualified Ribosome.Nvim.Api.Data as ApiData (vimCommand)
import Ribosome.Nvim.Api.IO (vimCallFunction)
import Ribosome.Nvim.Api.RpcCall (syncRpcCall)
import Ribosome.System.Time (sleep)

getChar ::
  NvimE e m =>
  MonadBaseControl IO m =>
  m InputEvent
getChar = do
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
