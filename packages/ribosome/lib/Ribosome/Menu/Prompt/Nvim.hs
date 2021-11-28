module Ribosome.Menu.Prompt.Nvim where

import Control.Concurrent.Async.Lifted (race)
import qualified Data.Text as Text (singleton, splitAt, uncons)
import qualified Streamly.Prelude as Stream

import Ribosome.Api.Atomic (atomic)
import Ribosome.Api.Window (redraw)
import Ribosome.Control.Monad.Ribo (MonadRibo, NvimE)
import Ribosome.Data.Text (escapeQuotes)
import Ribosome.Menu.Prompt.Data.Codes (decodeInputChar, decodeInputNum)
import Ribosome.Menu.Prompt.Data.InputEvent (InputEvent)
import qualified Ribosome.Menu.Prompt.Data.InputEvent as InputEvent (InputEvent (..))
import Ribosome.Menu.Prompt.Data.Prompt (Prompt (Prompt), PromptText (PromptText))
import Ribosome.Menu.Prompt.Data.PromptConfig (PromptInput (PromptInput))
import qualified Ribosome.Menu.Prompt.Data.PromptInputEvent as PromptInputEvent (PromptInputEvent (..))
import Ribosome.Menu.Prompt.Data.PromptRenderer (PromptRenderer (PromptRenderer))
import Ribosome.Msgpack.Encode (toMsgpack)
import Ribosome.Msgpack.Error (DecodeError)
import qualified Ribosome.Nvim.Api.Data as ApiData (vimCommand)
import Ribosome.Nvim.Api.IO (nvimInput, vimCallFunction, vimCommand, vimCommandOutput, vimGetOption, vimSetOption)
import Ribosome.Nvim.Api.RpcCall (RpcError, syncRpcCall)
import Ribosome.System.Time (sleep)

quitChar :: Char
quitChar =
  '†'

quitCharOrd :: Int
quitCharOrd =
  ord quitChar

getChar ::
  MonadIO m =>
  NvimE e m =>
  MonadBaseControl IO m =>
  MVar () ->
  m InputEvent
getChar quit =
  catchAs @RpcError InputEvent.Interrupt consume
  where
    consume =
      either pure event =<< race waitQuit (getchar [])
    waitQuit =
      readMVar quit *> getchar [True] $> InputEvent.Interrupt
    getchar =
      vimCallFunction "getchar" . fmap toMsgpack
    event (Right c) =
      pure $ InputEvent.Character (fromMaybe c (decodeInputChar c))
    event (Left 0) =
      pure InputEvent.NoInput
    event (Left num) | num == quitCharOrd =
      pure InputEvent.Interrupt
    event (Left num) =
      maybe (InputEvent.Unexpected num) InputEvent.Character <$> decodeInputNum num

getCharStream ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  Double ->
  PromptInput m
getCharStream interval =
  PromptInput \ quit -> do
    let
      run =
        check =<< Stream.fromEffect (getChar quit)
      check = \case
        InputEvent.Character a ->
          Stream.cons (PromptInputEvent.Character a) run
        InputEvent.Interrupt ->
          Stream.fromPure PromptInputEvent.Interrupt
        InputEvent.Error e ->
          Stream.fromPure (PromptInputEvent.Error e)
        InputEvent.NoInput ->
          Stream.before (sleep interval) run
        InputEvent.Unexpected _ ->
          run
    run

promptFragment :: Text -> Text -> [Text]
promptFragment hl text =
  ["echohl " <> hl, "echon '" <> escapeQuotes text <> "'"]

nvimRenderPrompt ::
  Monad m =>
  NvimE e m =>
  MonadDeepError e DecodeError m =>
  Prompt ->
  m ()
nvimRenderPrompt (Prompt cursor _ (PromptText text)) =
  void $ atomic calls
  where
    calls = syncRpcCall . ApiData.vimCommand <$> ("silent! redraw!" : (fragments >>= uncurry promptFragment))
    fragments =
      [
        ("RibosomePromptSign", sign),
        ("None", pre),
        ("RibosomePromptCaret", Text.singleton cursorChar),
        ("None", post)
        ]
    (pre, rest) =
      Text.splitAt cursor text
    (cursorChar, post) =
      fromMaybe (' ', "") (Text.uncons rest)
    sign =
      "% "

loopFunctionName :: Text
loopFunctionName =
  "RibosomeMenuLoop"

loopVarName :: Text
loopVarName =
  "ribosome_menu_looping"

newtype NvimPromptResources =
  NvimPromptResources {
    _guicursor :: Text
  }
  deriving stock (Eq, Show)

nvimAcquire ::
  NvimE e m =>
  m NvimPromptResources
nvimAcquire = do
  highlightSet <- catchAs @RpcError False $ True <$ vimCommandOutput "highlight RibosomePromptCaret"
  unless highlightSet $ vimCommand "highlight link RibosomePromptCaret TermCursor"
  res <- NvimPromptResources <$> vimGetOption "guicursor"
  vimSetOption "guicursor" (toMsgpack ("a:hor20" :: Text))
  () <- vimCallFunction "inputsave" []
  pure res

nvimRelease ::
  MonadIO m =>
  NvimE e m =>
  NvimPromptResources ->
  m ()
nvimRelease (NvimPromptResources gc) = do
  nvimInput "<esc>"
  vimSetOption "guicursor" (toMsgpack gc)
  redraw
  vimCommand "echon ''"
  vimCallFunction "inputrestore" []

nvimPromptRenderer ::
  MonadIO m =>
  NvimE e m =>
  MonadDeepError e DecodeError m =>
  PromptRenderer m
nvimPromptRenderer =
  PromptRenderer nvimAcquire nvimRelease nvimRenderPrompt
