module Ribosome.Menu.Prompt.Nvim where

import Conduit (ConduitT, yield)

import Ribosome.Control.Monad.Ribo (NvimE)
import Ribosome.Menu.Data.InputError (InputError)
import qualified Ribosome.Menu.Data.InputError as InputError (InputError(..))
import Ribosome.Menu.Data.InputEvent (InputEvent)
import qualified Ribosome.Menu.Data.InputEvent as InputEvent (InputEvent(..))
import Ribosome.Menu.Data.PromptEvent (PromptEvent)
import qualified Ribosome.Menu.Data.PromptEvent as PromptEvent (PromptEvent(..))
import Ribosome.Msgpack.Encode (toMsgpack)
import Ribosome.Nvim.Api.IO (vimCallFunction)
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
