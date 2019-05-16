{-# LANGUAGE TemplateHaskell #-}

module Ribosome.Menu.Prompt.Nvim where

import Conduit (ConduitT, yield)
import Data.MessagePack (Object)
import qualified Data.Text as Text (singleton, splitAt, uncons)

import Ribosome.Api.Atomic (atomic)
import Ribosome.Control.Monad.Ribo (Nvim, NvimE)
import Ribosome.Data.Text (escapeQuotes)
import Ribosome.Menu.Prompt.Data.InputError (InputError)
import qualified Ribosome.Menu.Prompt.Data.InputError as InputError (InputError(..))
import Ribosome.Menu.Prompt.Data.InputEvent (InputEvent)
import qualified Ribosome.Menu.Prompt.Data.InputEvent as InputEvent (InputEvent(..))
import Ribosome.Menu.Prompt.Data.Prompt (Prompt(Prompt))
import Ribosome.Menu.Prompt.Data.PromptEvent (PromptEvent)
import qualified Ribosome.Menu.Prompt.Data.PromptEvent as PromptEvent (PromptEvent(..))
import Ribosome.Msgpack.Encode (toMsgpack)
import Ribosome.Msgpack.Error (DecodeError)
import qualified Ribosome.Nvim.Api.Data as ApiData (vimCommand)
import Ribosome.Nvim.Api.IO (vimCallFunction)
import Ribosome.Nvim.Api.RpcCall (RpcError, syncRpcCall)
import Ribosome.System.Time (sleep)

getChar ::
  NvimE e m =>
  MonadDeepError e InputError m =>
  m InputEvent
getChar =
  event =<< vimCallFunction "getchar" [toMsgpack False]
  where
    event (Right c) =
      return (InputEvent.Character c)
    event (Left (0 :: Int)) =
      return InputEvent.NoInput
    event (Left num) =
      throwHoist (InputError.Unexpected num)

getCharC ::
  MonadIO m =>
  NvimE e m =>
  MonadDeepError e InputError m =>
  Double ->
  ConduitT Void PromptEvent m ()
getCharC interval =
  recurse
  where
    recurse =
      translate =<< lift getChar
    translate (InputEvent.Character a) =
      yield (PromptEvent.Character a)
    translate InputEvent.EOF =
      return ()
    translate _ =
      sleep interval *> recurse

promptFragment :: Text -> Text -> [Text]
promptFragment hl text =
  ["echohl " <> hl, "echon '" <> escapeQuotes text <> "'"]

data PromptRenderError =
  Decode DecodeError
  |
  Rpc RpcError
  deriving Show

deepPrisms ''PromptRenderError

nvimRenderPrompt ::
  Monad m =>
  Nvim m =>
  Prompt ->
  m ()
nvimRenderPrompt (Prompt cursor state text) =
  void $ runExceptT @PromptRenderError $ atomic calls
  where
    calls = syncRpcCall . ApiData.vimCommand <$> ("redraw" : (fragments >>= uncurry promptFragment))
    fragments =
      [("None", pre), ("RibosomePromptCaret", Text.singleton cursorChar), ("None", post)]
    (pre, rest) = Text.splitAt cursor text
    (cursorChar, post) = fromMaybe (' ', "") (Text.uncons rest)
